module Logic
  ( Gun(..), Rope(..)
  , Player(..), PlayerBody(..)
  , find_target, fire, release, tick_player, move
  , GameplayConfig(..)
  , Greeting(..)
  , ClientToServerMsg(..), ServerToClientMsg(..)
  , GraphNode(..), SerializablePlayer(..)
  , to_graphnode_map, update_player, serialize_player, obstacles_around, aos_to_ats
  , from_network_obs, NetworkObstacle(..)
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.UI.GLUT (GLdouble, Vector3(..))
import Math
import Debug.Trace
import Data.List
import MyGL ()
import MyUtil
import Data.Maybe
import Control.Monad.Fix
import TupleProjection
import Prelude hiding ((.))

data NetworkObstacle = NO [(V, V, V)] deriving (Show, Read)

data Greeting = Welcome GameplayConfig [NetworkObstacle] | PissOff String deriving (Show, Read)

from_network_obs :: [NetworkObstacle] → [AnnotatedObstacle]
from_network_obs = map (\(NO l) → annotate_obstacle $ (\(x, y, z) → annotate_triangle x y z) . l)

data ClientToServerMsg = FireAt Gun V | Release Gun | Spawn deriving (Read, Show)
data ServerToClientMsg =
  Players (Map String SerializablePlayer) |
    -- The Players message is used for "incremental" updates. For radical changes we use Teleport messages. This allows the client to "smooth out" movement between incremental changes, without having to go to great lengths to prevent smoothing out teleportation (which must not be smoothed!).
  Teleport String SerializablePlayer |
  TextMsg String
    deriving (Read, Show)

data GameplayConfig = GameplayConfig
  { rope_k, friction, shooting_speed, shooting_range :: GLdouble
  , gravity :: Vector3 GLdouble
  } deriving (Show, Read)

data Rope = Rope { rope_pos :: !V, rope_dir :: !V, rope_eta :: !Integer } deriving (Read, Show)

data PlayerBody = PlayerBody { pb_pos, pb_mov :: !V } deriving (Read, Show)

data Gun = LeftGun | RightGun deriving (Read, Show, Ord, Eq, Enum)

data SerializablePlayer = SerializablePlayer !PlayerBody (Map Gun Rope) Bool deriving (Read, Show)

data Player = Player
  { body :: !PlayerBody
  , guns :: Map Gun Rope
  , dead :: Bool
  , closest_obstacle :: GraphNode }

update_player :: Player → SerializablePlayer → Player
update_player p (SerializablePlayer x y z) = p { body = x, guns = y, dead = z }

serialize_player :: Player → SerializablePlayer
serialize_player (Player x y z _) = SerializablePlayer x y z

fire :: GameplayConfig → Gun → V → Player → Player
fire c g t p = p { guns = Map.insert g (Rope pos dir eta) (guns p) }
  where
   pos = pb_pos $ body p
   off = t <-> pos
   eta = round $ norm_2 off / shooting_speed c
   dir = off </> fromInteger eta

release :: Gun → Player → Player
release g p = p { guns = Map.delete g $ guns p }

aos_to_ats :: [AnnotatedObstacle] → [AnnotatedTriangle]
aos_to_ats = concat . (ao_triangles .)

rope_effect :: GameplayConfig → V → V
rope_effect c off = off </> (norm_2 off + rope_k c)

tick_player :: GameplayConfig → Player → Player
tick_player cfg p = trace (show $ pb_pos $ body p) $ if dead p then p else
  p { body = maybe newbody (flip PlayerBody (Vector3 0 0 0)) collision, guns = newguns {-, dead = isJust collision-}, closest_obstacle = new_closest }
  where
   oldpos@(Vector3 oldx oldy oldz) = pb_pos $ body p
   oldmov = pb_mov $ body p
   newguns = (. guns p) $ \r → case r of Rope pos dir (n + 1) → Rope (pos <+> dir) dir n; _ → r
   newbody = PlayerBody
    (oldpos <+> oldmov)
    ((gravity cfg <+> (Map.fold (\r m → case r of Rope pp _ 0 → m <+> rope_effect cfg (pp <-> oldpos); _ → m) oldmov $ guns p)) <*> friction cfg)
   new_closest = minimumByMeasure (dist_sqrd oldpos . ao_center . gn_obst) (take 40 $ neighbourhood $ closest_obstacle p)
   collision = if oldy < 0 then Just (Vector3 oldx 0 oldz) else $(project 1) .
    triangle_collision (aos_to_ats $ take 10 $ obstacles_around p) oldpos oldmov (\de _ → de > 0.1 && de < 1.1)

move :: V → Player → Player
move v p = p { body = (body p) { pb_pos = pb_pos (body p) <+> v } }

obstacles_around :: Player → [AnnotatedObstacle]
obstacles_around p = a : gn_obst . n where GraphNode a n = closest_obstacle p

find_target :: Player → GameplayConfig → V → V → Maybe V
find_target pl lcfg camera_pos gun_dir =
  $(project 1) . triangle_collision (aos_to_ats $ take 150 $ obstacles_around pl) camera_pos gun_dir (const $ (< square (shooting_range lcfg)) . dist_sqrd (pb_pos $ body pl))

data GraphNode = GraphNode
  { gn_obst :: AnnotatedObstacle
  , gn_neighbours :: [GraphNode] }

neighbourhood :: GraphNode → [GraphNode]
neighbourhood gn = gn : gn_neighbours gn

to_graphnode_map :: [AnnotatedObstacle] → Map V GraphNode
to_graphnode_map l =
  fix $ \m → Map.fromList $ (. l) $ \a →
  let (AnnotatedObstacle ac _) = a in
  (,) ac $ GraphNode a $ mapMaybe (flip Map.lookup m . fst) $
  sortBy (\c b → compare (snd c) (snd b)) $
  (\(AnnotatedObstacle x _) → (x, dist_sqrd ac x)) . l
