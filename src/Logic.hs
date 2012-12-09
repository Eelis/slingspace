{-# LANGUAGE RecordWildCards, ViewPatterns, UnicodeSyntax, TemplateHaskell, ScopedTypeVariables, PatternGuards, NamedFieldPuns, DeriveDataTypeable, LambdaCase #-}

module Logic
  ( Gun(..), Rope(..)
  , Player(..)
  , collisionPoint, fire, tickPlayer
  , GameplayConfig(..), GunConfig(..)
  , Life(..), lifeAfter, live, moments, lifeExpectancyUpto, birth, future, orAlternativeLife, reviseIfWise, keepTrying, positions, tryRandomAction, safeFuture
  , toFloor
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.Rendering.OpenGL.GL (GLdouble, Vector3(..))
import Math (V, GeometricObstacle(..), obstacleTriangles, Ray(..), collision, Cube(..), triangleCenter, AnnotatedTriangle, rayThrough)
import Data.AdditiveGroup ((^+^), (^-^), sumV)
import Data.VectorSpace ((^*), (^/), magnitude, magnitudeSq)
import Util ((.), randomItem, orElse)
import Data.Maybe (listToMaybe)
import Prelude hiding ((.))
import Obstacles (ObstacleTree)
import qualified Octree
import Control.Monad.Random (MonadRandom, getRandomR)
import Data.Typeable (Typeable)

data GunConfig = GunConfig
  { shootingSpeed, shootingRange :: GLdouble
  , ropeStrength :: GLdouble → GLdouble }

data GameplayConfig = GameplayConfig
  { gunConfig :: Gun → GunConfig
  , applyForce :: V → V
  } deriving Typeable

data Rope = Rope { rope_ray :: Ray, rope_eta :: !Integer } deriving (Read, Show)

data Gun = LeftGun | RightGun deriving (Read, Show, Ord, Eq, Enum)

data Player = Player
  { body :: !Ray
  , guns :: Map Gun Rope } -- todo: more appropriate data structure..

fireRope :: GunConfig → V → V → Rope
fireRope c t pos = Rope (Ray pos dir) eta
  where
   off = t ^-^ pos
   eta = round $ magnitude off / shootingSpeed c
   dir = off ^/ fromInteger eta

fire :: GunConfig → Gun → Maybe V → Player → Player
fire c g mt p = p { guns = case mt of
  Nothing → Map.delete g $ guns p
  Just t → Map.insert g (fireRope c t (rayOrigin (body p))) (guns p) }

ropeForce :: GunConfig → V → V
ropeForce GunConfig{ropeStrength} off =
  if p >= 0.1
    then off ^* (ropeStrength l / l)
    else Vector3 0 0 0
  where
    p = magnitudeSq off
    l = sqrt p

ropesForce :: GameplayConfig → Player → V
ropesForce GameplayConfig{gunConfig} Player{..} =
  sumV [ropeForce (gunConfig g) (p ^-^ rayOrigin body) | (g, Rope (Ray p _) 0) ← Map.toList guns]

progressRay :: Ray → Ray
progressRay r@Ray{..} = r { rayOrigin = rayOrigin ^+^ rayDirection }

tickPlayer :: ObstacleTree → GameplayConfig → Player → Either V Player
tickPlayer tree cfg p@Player{body=body@Ray{..}, ..} =
    case collisionPos of
      Just cp → Left cp
      Nothing → Right Player{
        body = Ray (rayOrigin ^+^ rayDirection) (applyForce cfg (rayDirection ^+^ ropesForce cfg  p)),
        guns = tickGun . guns }
  where
    Vector3 _ oldy _ = rayOrigin
    tickGun r@(Rope _ 0) = r
    tickGun (Rope ray n) = Rope (progressRay ray) (n - 1)
    collisionPos
      | oldy < 0 = Just (toFloor rayOrigin)
      | otherwise = collisionPoint body (Octree.query body tree >>= obstacleTriangles)
      -- using rayOrigin instead of body as the query only reduces the benchmark runtime by about 5% (and would of course be inaccurate)

collisionPoint :: Ray → [AnnotatedTriangle] → Maybe V
collisionPoint r@Ray{..} t = (rayOrigin ^+^) . (rayDirection ^*) . fst . collision r t

data Life = Life Player Life | Death V

moments :: Life → [Player]
moments (Death _) = []
moments (Life p l) = p : moments l

lifeExpectancyUpto :: Integer → Life → Integer
lifeExpectancyUpto m = go 0
  where
    go n l
      | Life _ l' ← l, n /= m = go (n+1) l'
      | otherwise = n

lifeAfter :: ObstacleTree → GameplayConfig → Player → Life
lifeAfter tree cfg = go
  where go p = either Death (\q → Life q (go q)) $ tickPlayer tree cfg p

live :: ObstacleTree → GameplayConfig → Player → Life
live tree cfg p = Life p (lifeAfter tree cfg p)

birth :: Life → Maybe Player -- Nothing if stillborn
birth = listToMaybe . moments

toFloor :: Num a ⇒ Vector3 a → Vector3 a
toFloor (Vector3 x _ z) = Vector3 x 0 z

future :: Life → Life
future (Life _ l) = l
future l = l

safeFuture :: ObstacleTree → GameplayConfig → Life → Life
  -- i.e. a future that isn't death
safeFuture t c (Death v) = live t c $ Player (Ray v (Vector3 0 0 0)) Map.empty
safeFuture t c (Life p (Death v)) = live t c $ Player (Ray v (Vector3 0 0 0)) (guns p)
safeFuture _ _ (Life _ l) = l

orAlternativeLife :: Life → Life → Life
orAlternativeLife (Death _) l = l -- oops, died
orAlternativeLife l _ = l

reviseIfWise :: Functor m ⇒ (Life → m Life) → Life → m Life
reviseIfWise f x = fmap (`orAlternativeLife` x) (f x)

mapFuture :: (Monad m, Functor m) ⇒ (Life → m Life) → Life → m Life
mapFuture f (Life a b) = Life a `fmap` f b
mapFuture _ d = return d

keepTrying :: (Monad m, Functor m) ⇒ (Life → m Life) → Life → m Life
keepTrying f l = reviseIfWise f l >>= mapFuture (keepTrying f)

positions :: Life → [V]
positions = map (rayOrigin . body) . moments

randomAction :: (Functor m, MonadRandom m) ⇒ ObstacleTree → GunConfig → Player → m Player
randomAction tree cfg now = do
  i :: Int ← getRandomR (0, 10)
  if i == 0
    then return $ fire cfg LeftGun Nothing now
    else randomItem nearby >>= \case
      Nothing → return now
      Just (triangleCenter → c) → return $ fire cfg LeftGun (Just (collisionPoint (rayThrough p c) nearby `orElse` c)) now
  where
    p = rayOrigin (body now)
    s = shootingRange cfg
    nearby = Octree.query (Cube (p ^-^ Vector3 s s s) (p ^+^ Vector3 s s s)) tree >>= obstacleTriangles

randomLife :: (Functor m, MonadRandom m) ⇒ ObstacleTree → GameplayConfig → Player → m Life
randomLife tree gpCfg = fmap (live tree gpCfg) . randomAction tree (gunConfig gpCfg LeftGun)

tryRandomAction :: (Functor m, MonadRandom m) ⇒
  (Life → Life → Bool) → ObstacleTree → GameplayConfig → Life → m Life
tryRandomAction _ _ _ d@(Death _) = return d -- hunter-to-be already dead
tryRandomAction cmp tree gpCfg current@(Life now _) = do
  alternative ← randomLife tree gpCfg now
  return $ if future alternative `cmp` future current
    then alternative
    else Death (rayOrigin $ body $ now)
