{-# LANGUAGE RecordWildCards, ViewPatterns, UnicodeSyntax, TemplateHaskell, ScopedTypeVariables, PatternGuards, NamedFieldPuns #-}

module Logic
  ( Gun(..), Rope(..)
  , Player(..)
  , findTarget, fire, release, tickPlayer, move, gunConfigFor
  , GameplayConfig(..), GunConfigs, GunConfig(shootingRange)
  , Life(..), lifeAfter, live, moments, lifeExpectancyUpto, birth, future, immortalize, orAlternativeLife, reviseIfWise, keepTrying, positions, tryRandomAction
  , toFloor
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.UI.GLUT (GLdouble, Vector3(..))
import Math ((<+>), (<->), (</>), (<*>), norm_2, V, GeometricObstacle(..), obstacleTriangles, dist_sqrd, square, Ray(..), collision, Cube(..), triangleCenter)
import MyGL ()
import MyUtil ((.), randomItem)
import Data.Maybe (listToMaybe, fromJust)
import Prelude hiding ((.))
import Obstacles (ObstacleTree)
import qualified Octree
import Control.Monad.Random (MonadRandom, getRandomR)

data GunConfig = GunConfig
  { ropeForceScale, ropeForceExponent, shootingSpeed, shootingRange :: GLdouble }
  deriving (Show, Read)

type GunConfigs = Map Gun GunConfig

data GameplayConfig = GameplayConfig
  { gunConfigs :: GunConfigs
  , friction :: GLdouble, gravity :: V
  } deriving (Show, Read)

data Rope = Rope { rope_ray :: Ray, rope_eta :: !Integer } deriving (Read, Show)

data Gun = LeftGun | RightGun deriving (Read, Show, Ord, Eq, Enum)

data Player = Player
  { body :: !Ray
  , guns :: Map Gun Rope } -- todo: more appropriate data structure..

gunConfigFor :: Gun -> GunConfigs -> GunConfig
gunConfigFor g = fromJust . Map.lookup g

fireRope :: GunConfig → V → V → Rope
fireRope c t pos = Rope (Ray pos dir) eta
  where
   off = t <-> pos
   eta = round $ norm_2 off / shootingSpeed c
   dir = off </> fromInteger eta

fire :: GunConfig → Gun → V → Player → Player
fire c g t p = p { guns = Map.insert g (fireRope c t (rayOrigin (body p))) (guns p) }

release :: Gun → Player → Player
release g p = p { guns = Map.delete g $ guns p }

rope_effect :: GunConfig → V → V
rope_effect GunConfig{..} off = (off </> l) <*> (ropeForceScale * (l ** ropeForceExponent))
  where l = norm_2 off

progressRay :: Ray → Ray
progressRay r@Ray{..} = r { rayOrigin = rayOrigin <+> rayDirection }

tickPlayer :: ObstacleTree → GameplayConfig → Player → Either V Player
tickPlayer tree cfg@GameplayConfig{gunConfigs} Player{body=body@Ray{..}, ..} =
    case collisionPos of
      Just cp -> Left cp
      Nothing -> Right $ Player{ body = newBody, guns = tickGun . guns }
  where
    Vector3 _ oldy _ = rayOrigin
    tickGun r@(Rope _ 0) = r
    tickGun (Rope ray n) = Rope (progressRay ray) (n - 1)
    newBody = Ray
        (rayOrigin <+> rayDirection)
        ((gravity cfg <+> Map.foldrWithKey (\g r m → case r of Rope (Ray pp _) 0 → m <+> rope_effect (gunConfigFor g gunConfigs) (pp <-> rayOrigin); _ → m) rayDirection guns) <*> friction cfg)
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

lifeExpectancyUpto :: Integer -> Life -> Integer
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

future :: Life -> Life
future l@(Death _) = l
future (Life _ l) = l

immortalize :: ObstacleTree → GameplayConfig -> Life -> Life
immortalize t c u = case u of
    Life p l → go p l
    Death _ → error "can't immortalize the dead"
  where
    go _ (Life p l) = Life p (go p l)
    go p (Death v) = Life p' (go p' (lifeAfter t c p'))
      where p' = Player (Ray v (Vector3 0 0 0)) (guns p)

orAlternativeLife :: Life -> Life -> Life
orAlternativeLife (Death _) l = l -- oops, died
orAlternativeLife l _ = l

reviseIfWise :: Functor m => (Life -> m Life) → Life → m Life
reviseIfWise f x = fmap (`orAlternativeLife` x) (f x)

mapFuture :: (Monad m, Functor m) => (Life -> m Life) -> Life -> m Life
mapFuture _ d@(Death _) = return d
mapFuture f (Life a b) = Life a `fmap` f b

keepTrying :: (Monad m, Functor m) => (Life -> m Life) -> Life -> m Life
keepTrying f l = reviseIfWise f l >>= mapFuture (keepTrying f)

positions :: Life -> [V]
positions = map (rayOrigin . body) . moments

randomAction :: (Functor m, MonadRandom m) => ObstacleTree → GunConfig → Player → m Player
randomAction tree cfg now = do
  i :: Int <- getRandomR (0, 10)
  if i == 0
    then return $ release LeftGun now
    else do
      c <- randomItem nearby >>= randomItem . map triangleCenter . obstacleTriangles
      return $ fire cfg LeftGun c now
  where
    s = 3000 -- todo: base on shootingRange
    here = Cube (rayOrigin (body now) <-> Vector3 s s s) (s*2)
    nearby = Octree.query here tree

randomLife :: (Functor m, MonadRandom m) => ObstacleTree → GameplayConfig → Player → m Life
randomLife tree gpCfg = fmap (live tree gpCfg) . randomAction tree (gunConfigFor LeftGun (gunConfigs gpCfg))

tryRandomAction :: (MonadRandom m, Functor m) => (Life -> Life -> Bool) -> ObstacleTree -> GameplayConfig -> Life -> m Life
tryRandomAction _ _ _ d@(Death _) = return d -- hunter-to-be already dead
tryRandomAction cmp tree gpCfg current@(Life now _) = do
  alternative <- randomLife tree gpCfg now
  return $ if future alternative `cmp` future current
    then alternative
    else Death (rayOrigin $ body $ now)

