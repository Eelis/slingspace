{-# LANGUAGE RecordWildCards, ViewPatterns, UnicodeSyntax, TemplateHaskell, ScopedTypeVariables, PatternGuards #-}

module Logic
  ( Gun(..), Rope(..)
  , Player(..)
  , find_target, fire, release, tickPlayer, move
  , GameplayConfig(..)
  , Greeting(..)
  , ClientToServerMsg(..), ServerToClientMsg(..)
  , SerializablePlayer(..)
  , update_player, serialize_player
  , from_network_obs, NetworkObstacle(..)
  , toFloor
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.UI.GLUT (GLdouble, Vector3(..))
import Math ((<+>), (<->), (</>), (<*>), annotateObstacle, annotateTriangle, norm_2, V, GeometricObstacle(..), obstacleTriangles, dist_sqrd, square, Ray(..), collision)
import MyGL ()
import MyUtil ((.))
import Prelude hiding ((.))
import qualified Octree

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

tickPlayer :: Octree.CubeBox GeometricObstacle → GameplayConfig → Player → Player
tickPlayer tree cfg p@Player{body=body@Ray{..}, ..} = if dead then p else p
    { body = newBody, guns = tickGun . guns {-, dead = isJust collision-} }
  where
    Vector3 _ oldy _ = rayOrigin
    tickGun r@(Rope _ 0) = r
    tickGun (Rope ray n) = Rope (progressRay ray) (n - 1)
    newBody
      | Just d ← collisionPos = Ray d (Vector3 0 0 0)
      | otherwise = Ray
        (rayOrigin <+> rayDirection)
        ((gravity cfg <+> (Map.fold (\r m → case r of Rope (Ray pp _) 0 → m <+> rope_effect cfg (pp <-> rayOrigin); _ → m) rayDirection guns)) <*> friction cfg)
    collisionPos
      | oldy < 0 = Just (toFloor rayOrigin)
      | otherwise = (\(_, x, _) -> x) . collision (body, \(de::GLdouble) (_::V) → de > 0.1 && de < 1.1) (filteredObstacles >>= obstacleTriangles)
    filteredObstacles = Octree.query body tree
      -- using rayOrigin instead of body as the query only reduces the benchmark runtime by about 5% (and would of course be inaccurate) 

move :: V → Player → Player
move v p@Player{..} = p { body = body { rayOrigin = rayOrigin body <+> v } }

find_target :: Octree.CubeBox GeometricObstacle → Player → GameplayConfig → Ray → Maybe V
find_target tree player GameplayConfig{..} gunRay@(Ray gunOrigin gunDirection) =
  (\(_, x, _) -> x) . collision (gunRay, \(_::GLdouble) (v::V) → dist_sqrd (rayOrigin $ body player) v < square shooting_range)
    (filteredObstacles >>= obstacleTriangles)
  where
    filteredObstacles = Octree.query longGunRay tree
    longGunRay = Ray gunOrigin (gunDirection <*> shooting_range) -- todo

toFloor :: Num a ⇒ Vector3 a → Vector3 a
toFloor (Vector3 x _ z) = Vector3 x 0 z
