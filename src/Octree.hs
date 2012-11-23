{-# LANGUAGE RecordWildCards, NamedFieldPuns, FlexibleContexts, PatternGuards, BangPatterns #-}

module Octree (Box, CubeBox, empty, query, insert, toList, fromList, subs) where

import Graphics.Rendering.OpenGL.GL (Vector3(..), GLdouble)
import Math (Collision(collision), FitsInCube(fitsInCube), Cube(..), (<*>), (<+>))
import MyUtil (orElse)
import Control.DeepSeq (NFData)

import Data.Vector (Vector)
import qualified Data.Vector as Vector

type Subs a = Vector (Cube, Maybe (Box a))

subIds :: [Vector3 GLdouble]
subIds = 
  [ Vector3 0 0 0
  , Vector3 0 0 1
  , Vector3 0 1 0
  , Vector3 0 1 1
  , Vector3 1 0 0
  , Vector3 1 0 1
  , Vector3 1 1 0
  , Vector3 1 1 1 ] -- todo: make vector

mapFirst :: [a] -> (a -> Maybe a) -> Maybe [a]
mapFirst [] _ = Nothing
mapFirst (h : t) f
  | Just h' <- f h = Just $ h' : t
  | otherwise = (h:) `fmap` mapFirst t f

mapFirstV :: Vector a -> (a -> Maybe a) -> Maybe (Vector a)
mapFirstV v f = Vector.fromList `fmap` mapFirst (Vector.toList v) f
  -- horribly inefficient, but only used in tree construction

data Box a = Box
  { objects :: ![a] -- which don't fit into smaller boxes
  , subBoxes :: !(Subs a) }

instance NFData a => NFData (Box a)

showBox :: Show a => Box a -> [String]
showBox Box{..} =
  ("[" ++ replicate (length objects) '+' {-show objects-} ++ "]") : map ("  " ++)
    (concatMap (maybe [] showBox . snd) (Vector.toList subBoxes))

instance Show a => Show (Box a) where
  show = unlines . showBox

type CubeBox a = (Cube, Box a)

boxToList :: Box a -> [a]
boxToList Box{..} = objects ++ concatMap (maybe [] boxToList . snd) (Vector.toList subBoxes)

toList :: CubeBox a -> [a]
toList = boxToList . snd

emptyBox :: Cube -> Box a
emptyBox Cube{..} =
    Box [] $ Vector.fromList [(Cube (cubeCorner <+> (i <*> subOff)) subSize, Nothing) | i <- subIds]
  where
    subOff = cubeSize / 3
    subSize = subOff * 2

empty :: Cube -> CubeBox a
empty !c = (c, emptyBox c)

doInsert :: FitsInCube a => a -> Cube -> Maybe (Box a) -> Maybe (Box a)
doInsert obj cube mb
  | not (obj `fitsInCube` cube) = Nothing
  | otherwise = Just $ case mapFirstV subBoxes f of
        Nothing -> Box (obj : objects) subBoxes
        Just subBoxes' -> Box objects subBoxes'
  where
    Box{objects,subBoxes} = mb `orElse` emptyBox cube
    f (c', msb) = case doInsert obj c' msb of
      Nothing -> Nothing
      Just x -> Just (c', Just x)

insert :: (Show a, FitsInCube a) => CubeBox a -> a -> CubeBox a
insert (c, b) o
  | Just b' <- doInsert o c (Just b) = (c, b')
  | otherwise = error $ "object " ++ show o ++ " did not fit in cube " ++ show c

fromList :: (Show a, FitsInCube a) => Cube -> [a] -> CubeBox a
fromList c = foldl insert (empty c)

subs :: CubeBox a -> [CubeBox a]
subs (_, Box{subBoxes}) = [ (c, b) | (c, Just b) <- Vector.toList subBoxes ]

query :: Collision q Cube Bool => q -> CubeBox a -> [a]
query !q = go
  where
    go!(c, Box{..})
      | not (collision q c) = []
      | otherwise = Vector.foldl' f objects subBoxes
      where
        f a (_, Nothing) = a
        f a (c', Just b') = a ++ go (c', b')

