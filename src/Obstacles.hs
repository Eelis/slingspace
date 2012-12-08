{-# LANGUAGE UnicodeSyntax, TypeOperators, ScopedTypeVariables #-}

module Obstacles (randomObs, ObstacleTree, grow) where

import Util ((.))
import Math (V,  GeometricObstacle(..), randomVector3, annotateObstacle, annotateTriangle, Cube(..), collide)
import Data.AdditiveGroup ((^+^), (^-^))
import Data.VectorSpace ((^*), normalized)
import Data.Cross (cross3)
import Control.Arrow (second)
import Graphics.Rendering.OpenGL.GL (GLdouble, Vector3(..))
import Prelude hiding ((.))
import Control.Monad.Random (MonadRandom(..))
import qualified Octree

type ObstacleTree = Octree.CubeBox Cube GeometricObstacle

bigCube :: Cube
bigCube = Cube (Vector3 (-m) (-m) (-m)) (Vector3 m m m)
  where m = 200000

intersects :: GeometricObstacle → ObstacleTree → Bool
intersects o = any (collide o) . Octree.query o

grow :: [GeometricObstacle] → (ObstacleTree, [GeometricObstacle])
  -- the second component of the result pair is a filtered list of the obstacles, in which intersecting ones have been removed
grow = go (Octree.empty bigCube)
  where
    go t [] = (t, [])
    go t (x:xs)
      | x `intersects` t = go t xs
      | otherwise = second (x :) $ go (Octree.insert t x) xs

aboveSurface :: V → V
aboveSurface (Vector3 x y z) = Vector3 x (max 0 y) z

randomObs :: (Functor m, MonadRandom m) ⇒ GLdouble → V → m GeometricObstacle
randomObs size center = do
  let q = aboveSurface . (center ^+^) . randomVector3 size
  a ← q; b ← q; c ← q
  let w = aboveSurface $ a ^-^ (normalized (cross3 (b ^-^ a) (c ^-^ a)) ^* size)
  return $ annotateObstacle [at a b c, at b a w, at a c w, at c b w]
 where at = annotateTriangle
