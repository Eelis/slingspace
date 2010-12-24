{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Logic
  ( Gun(..), Rope(..)
  , Player(..)
  , find_target, fire, release, tick_player, move
  , GameplayConfig(..)
  , Greeting(..)
  , ClientToServerMsg(..), ServerToClientMsg(..)
  , GraphNode(..), SerializablePlayer(..)
  , to_graphnode_map, update_player, serialize_player
  , from_network_obs, NetworkObstacle(..)
  , toFloor
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.UI.GLUT (GLdouble, Vector3(..))
import Math ((<+>), (<->), (</>), (<*>), annotateObstacle, annotateTriangle, norm_2, V, GeometricObstacle(..), obstacleTriangles, dist_sqrd, square, Ray(..), collision)
import Data.List (sortBy)
import MyGL ()
import MyUtil ((.), minimumByMeasure)
import Data.Maybe (mapMaybe)
import Control.Monad.Fix (fix)
import TupleProjection (project)
import Prelude hiding ((.))

data NetworkObstacle = NO [(V, V, V)] deriving (Show, Read)

data Greeting = Welcome GameplayConfig [NetworkObstacle] | PissOff String deriving (Show, Read)

from_network_obs :: [NetworkObstacle] → [GeometricObstacle]
from_network_obs = map (\(NO l) → annotateObstacle $ (\(x, y, z) → annotateTriangle x y z) . l)

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

data Rope = Rope { rope_ray :: Ray, rope_eta :: !Integer } deriving (Read, Show)

data Gun = LeftGun | RightGun deriving (Read, Show, Ord, Eq, Enum)

data SerializablePlayer = SerializablePlayer !Ray (Map Gun Rope) Bool deriving (Read, Show)

data Player = Player
  { body :: !Ray
  , guns :: Map Gun Rope
  , dead :: Bool }

update_player :: Player → SerializablePlayer → Player
update_player p (SerializablePlayer x y z) = p { body = x, guns = y, dead = z }

serialize_player :: Player → SerializablePlayer
serialize_player = error "broken" -- (Player x y z _) = SerializablePlayer x y z

fire :: GameplayConfig → Gun → V → Player → Player
fire c g t p = p { guns = Map.insert g (Rope (Ray pos dir) eta) (guns p) }
  where
   pos = rayOrigin $ body p
   off = t <-> pos
   eta = round $ norm_2 off / shooting_speed c
   dir = off </> fromInteger eta

release :: Gun → Player → Player
release g p = p { guns = Map.delete g $ guns p }

rope_effect :: GameplayConfig → V → V
rope_effect c off = off </> (norm_2 off + rope_k c)

progressRay :: Ray → Ray
progressRay r@Ray{..} = r { rayOrigin = rayOrigin <+> rayDirection }

tick_player :: [GeometricObstacle] → GameplayConfig → Player → Player
tick_player collidable cfg p = if dead p then p else
  p { body = maybe newbody (flip Ray (Vector3 0 0 0)) co, guns = newguns {-, dead = isJust collision-} }
  where
   oldpos@(Vector3 oldx oldy oldz) = rayOrigin $ body p
   oldmov = rayDirection $ body p
   newguns = (. guns p) $ \r → case r of Rope ray (n + 1) → Rope (progressRay ray) n; _ → r
   newbody = Ray
    (oldpos <+> oldmov)
    ((gravity cfg <+> (Map.fold (\r m → case r of Rope (Ray pp _) 0 → m <+> rope_effect cfg (pp <-> oldpos); _ → m) oldmov $ guns p)) <*> friction cfg)
   co = if oldy < 0 then Just (toFloor oldpos) else $(project 1) . collision (Ray oldpos oldmov, \(de::GLdouble) (_::V) → de > 0.1 && de < 1.1) (collidable >>= obstacleTriangles)

move :: V → Player → Player
move v p@Player{..} = p { body = body { rayOrigin = rayOrigin body <+> v } }

-- obstacles_around :: Player → [GeometricObstacle]
-- obstacles_around = (gn_obst .) . neighbourhood . closest_obstacle

find_target :: [GeometricObstacle] → Player → GameplayConfig → Ray → Maybe V
find_target shootableObstacles player lcfg@GameplayConfig{..} gunRay =
  $(project 1) . collision (gunRay, \(_::GLdouble) (v::V) → dist_sqrd (rayOrigin $ body player) v < square shooting_range) (shootableObstacles >>= obstacleTriangles)

data GraphNode = GraphNode
  { gn_obst :: GeometricObstacle
  , gn_neighbours :: [GraphNode] }

neighbourhood :: GraphNode → [GraphNode]
neighbourhood gn = gn : gn_neighbours gn

to_graphnode_map :: [GeometricObstacle] → Map V GraphNode
to_graphnode_map l =
  fix $ \m → Map.fromList $ (. l) $ \a →
  let (GeometricObstacle ac _) = a in
  (,) ac $ GraphNode a $ mapMaybe (flip Map.lookup m . fst) $
  sortBy (\c b → compare (snd c) (snd b)) $
  (\(GeometricObstacle x _) → (x, dist_sqrd ac x)) . l

toFloor :: Num a ⇒ Vector3 a → Vector3 a
toFloor (Vector3 x _ z) = Vector3 x 0 z
