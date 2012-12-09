{-# LANGUAGE UnicodeSyntax, RecordWildCards, NamedFieldPuns, FlexibleContexts, PatternGuards, BangPatterns, FlexibleInstances #-}

module Octree (Box, CubeBox, empty, query, insert, toList, fromList, subs) where

import Graphics.Rendering.OpenGL.GL (Vector3(..))
import Math (Collision(collide), FitsIn(fitsIn), Cube(..))
import Data.AdditiveGroup ((^+^), (^-^))
import Data.VectorSpace ((^*), (^/))
import Util (orElse)
import Control.DeepSeq (NFData)

import Data.Vector (Vector)
import qualified Data.Vector as Vector

type Subs c a = Vector (c, Maybe (Box c a))

class Splittable t where split :: t → [t]

instance Splittable Cube where
  split Cube{..} =
      [Cube p (p ^+^ subSize) | i ← subIds, let p = cubeLoCorner ^+^ (t i subOff)]
    where
      subOff = (cubeHiCorner ^-^ cubeLoCorner) ^/ 3
      subSize = subOff ^* 2
      t (Vector3 x y z) (Vector3 a b c) = Vector3 (x * a) (y * b) (z * c)
      subIds =
        [ Vector3 0 0 0
        , Vector3 0 0 1
        , Vector3 0 1 0
        , Vector3 0 1 1
        , Vector3 1 0 0
        , Vector3 1 0 1
        , Vector3 1 1 0
        , Vector3 1 1 1 ]

mapFirst :: [a] → (a → Maybe a) → Maybe [a]
mapFirst [] _ = Nothing
mapFirst (h : t) f
  | Just h' ← f h = Just $ h' : t
  | otherwise = (h:) `fmap` mapFirst t f

mapFirstV :: Vector a → (a → Maybe a) → Maybe (Vector a)
mapFirstV v f = Vector.fromList `fmap` mapFirst (Vector.toList v) f
  -- horribly inefficient, but only used in tree construction

data Box c a = Box
  { objects :: ![a] -- which don't fit into smaller boxes
  , subBoxes :: !(Subs c a) }

instance (NFData c, NFData a) ⇒ NFData (Box c a)

type CubeBox c a = (c, Box c a)

boxToList :: Box c a → [a]
boxToList Box{..} = objects ++ concatMap (maybe [] boxToList . snd) (Vector.toList subBoxes)

toList :: CubeBox c a → [a]
toList = boxToList . snd

emptyBox :: Splittable t ⇒ t → Box t a
emptyBox c = Box [] $ Vector.fromList [(c', Nothing) | c' ← split c]

empty :: Splittable t ⇒ t → CubeBox t a
empty !c = (c, emptyBox c)

doInsert :: (Splittable c, FitsIn a c) ⇒ a → c → Maybe (Box c a) → Maybe (Box c a)
doInsert obj cube mb
  | not (obj `fitsIn` cube) = Nothing
  | otherwise = Just $ case mapFirstV subBoxes f of
        Nothing → Box (obj : objects) subBoxes
        Just subBoxes' → Box objects subBoxes'
  where
    Box{objects,subBoxes} = mb `orElse` emptyBox cube
    f (c', msb) = case doInsert obj c' msb of
      Nothing → Nothing
      Just x → Just (c', Just x)

insert :: (Splittable c, FitsIn a c) ⇒ CubeBox c a → a → CubeBox c a
insert (c, b) o
  | Just b' ← doInsert o c (Just b) = (c, b')
  | otherwise = error "object did not fit in cube"

fromList :: (FitsIn a Cube) ⇒ Cube → [a] → CubeBox Cube a
fromList c = foldl insert (empty c)

subs :: CubeBox c a → [CubeBox c a]
subs (_, Box{subBoxes}) = [ (c, b) | (c, Just b) ← Vector.toList subBoxes ]

-- collision detection may yield false positives, but no false negatives

query :: Collision q c x ⇒ q → CubeBox c a → [a]
query !q = go
  where
    go!(c, Box{..})
      | not (collide q c) = []
      | otherwise = Vector.foldl' f objects subBoxes
      where
        f a (_, Nothing) = a
        f a (c', Just b') = a ++ go (c', b')
