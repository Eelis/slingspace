{-# LANGUAGE RecordWildCards, ViewPatterns, UnicodeSyntax, TemplateHaskell, ScopedTypeVariables, PatternGuards, NamedFieldPuns #-}

module Logic
  ( Gun(..), Rope(..)
  , Player(..)
  , findTarget, fire, release, tickPlayer, move
  , GameplayConfig(..)
  , Greeting(..)
  , ClientToServerMsg(..), ServerToClientMsg(..)
  , SerializablePlayer(..)
  , update_player, serialize_player
  , from_network_obs, NetworkObstacle(..)
  , Life(..), lifeAfter, live, moments, lifeExpectancyUpto, birth
  , toFloor
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.UI.GLUT (GLdouble, Vector3(..))
import Math ((<+>), (<->), (</>), (<*>), annotateObstacle, annotateTriangle, norm_2, V, GeometricObstacle(..), obstacleTriangles, dist_sqrd, square, Ray(..), collision)
import MyGL ()
import MyUtil ((.))
import Data.Maybe (listToMaybe)
import Prelude hiding ((.))
import Obstacles (ObstacleTree)
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

data SerializablePlayer = SerializablePlayer !Ray (Map Gun Rope) deriving (Read, Show)

data Player = Player
  { body :: !Ray
  , guns :: Map Gun Rope } -- todo: more appropriate data structure..

update_player :: Player → SerializablePlayer → Player
update_player p (SerializablePlayer x y) = p { body = x, guns = y }

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

tickPlayer :: ObstacleTree → GameplayConfig → Player → Either V Player
tickPlayer tree cfg Player{body=body@Ray{..}, ..} =
    case collisionPos of
      Just cp -> Left cp
      Nothing -> Right $ Player{ body = newBody, guns = tickGun . guns }
  where
    Vector3 _ oldy _ = rayOrigin
    tickGun r@(Rope _ 0) = r
    tickGun (Rope ray n) = Rope (progressRay ray) (n - 1)
    newBody = Ray
        (rayOrigin <+> rayDirection)
        ((gravity cfg <+> (Map.fold (\r m → case r of Rope (Ray pp _) 0 → m <+> rope_effect cfg (pp <-> rayOrigin); _ → m) rayDirection guns)) <*> friction cfg)
    collisionPos
      | oldy < 0 = Just (toFloor rayOrigin)
      | otherwise = (\(_, x, _) -> x) . collision (body, \(de::GLdouble) (_::V) → de > 0.1 && de < 1.1) (filteredObstacles >>= obstacleTriangles)
    filteredObstacles = Octree.query body tree
      -- using rayOrigin instead of body as the query only reduces the benchmark runtime by about 5% (and would of course be inaccurate) 

move :: V → Player → Player
move v p@Player{..} = p { body = body { rayOrigin = rayOrigin body <+> v } }

findTarget :: ObstacleTree → V → GLdouble → Ray → Maybe V
findTarget tree playerPos shooting_range gunRay@(Ray gunOrigin gunDirection) =
  (\(_, x, _) -> x) . collision (gunRay, \(_::GLdouble) (v::V) → dist_sqrd playerPos v < square shooting_range)
    (filteredObstacles >>= obstacleTriangles)
  where
    filteredObstacles = Octree.query longGunRay tree
    longGunRay = Ray gunOrigin (gunDirection <*> shooting_range) -- todo

data Life = Life Player Life | Death V

moments :: Life -> [Player]
moments (Death _) = []
moments (Life p l) = p : moments l

lifeExpectancyUpto :: Int -> Life -> Int
lifeExpectancyUpto m = go 0
  where
    go n l
      | Life _ l' <- l, n /= m = go (n+1) l'
      | otherwise = n

lifeAfter :: ObstacleTree → GameplayConfig -> Player -> Life
lifeAfter tree cfg = go
  where go p = either Death (\q -> Life q (go q)) $ tickPlayer tree cfg p

live :: ObstacleTree → GameplayConfig -> Player -> Life
live tree cfg p = Life p (lifeAfter tree cfg p)

birth :: Life -> Maybe Player -- Nothing if stillborn
birth = listToMaybe . moments

toFloor :: Num a ⇒ Vector3 a → Vector3 a
toFloor (Vector3 x _ z) = Vector3 x 0 z
