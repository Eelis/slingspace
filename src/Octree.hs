{-# LANGUAGE RecordWildCards, NamedFieldPuns, FlexibleContexts, PatternGuards, BangPatterns #-}

module Octree (Box, CubeBox, empty, query, insert, toList, fromList, subs) where

import Graphics.Rendering.OpenGL.GL (Vector3(..), GLdouble)
import Math (Collision(collision), FitsInCube(fitsInCube), Cube(..), (<*>), (<+>))
import MyUtil (orElse)

-- import Debug.Trace (trace)

type Subs a = [Maybe (Box a)]

subIds :: [Vector3 GLdouble]
subIds = 
  [ Vector3 0 0 0
  , Vector3 0 0 1
  , Vector3 0 1 0
  , Vector3 0 1 1
  , Vector3 1 0 0
  , Vector3 1 0 1
  , Vector3 1 1 0
  , Vector3 1 1 1 ]

mapFirst :: [a] -> (a -> Maybe a) -> Maybe [a]
mapFirst [] _ = Nothing
mapFirst (h : t) f
  | Just h' <- f h = Just $ h' : t
  | otherwise = (h:) `fmap` mapFirst t f

data Box a = Box
  { objects :: ![a] -- which don't fit into smaller boxes
  , subBoxes :: !(Subs a) }


showBox :: Show a => Box a -> [String]
showBox Box{..} =
  ("[" ++ replicate (length objects) '+' {-show objects-} ++ "]") : map ("  " ++)
    (concatMap (\mb -> case mb of Nothing -> []; Just b -> showBox b) subBoxes)


instance Show a => Show (Box a) where
  show = unlines . showBox


type CubeBox a = (Cube, Box a)


boxToList :: Box a -> [a]
boxToList Box{..} = objects ++ concatMap (\mb -> case mb of Nothing -> []; Just b -> boxToList b) subBoxes


toList :: CubeBox a -> [a]
toList = boxToList . snd

emptySubs :: Subs a
emptySubs = replicate (length subIds) Nothing

emptyBox :: Box a
emptyBox = Box [] emptySubs

empty :: Cube -> CubeBox a
empty !c = (c, emptyBox)

subCubes :: Cube -> [Cube]
subCubes Cube{..} = [Cube (cubeCorner <+> (i <*> subOff)) subSize | i <- subIds ]
  where
    subOff = cubeSize / 3
    subSize = subOff * 2

{-# INLINE subCubes #-}

doInsert :: FitsInCube a => a -> Cube -> Maybe (Box a) -> Maybe (Box a)
doInsert obj cube mb
  | not (obj `fitsInCube` cube) = Nothing
  | otherwise = Just $ case mapFirst (zip (subCubes cube) subBoxes) f of
        Nothing -> Box (obj : objects) subBoxes
        Just subBoxes' -> Box objects (map snd subBoxes')
  where
    Box{objects,subBoxes} = mb `orElse` emptyBox
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
subs (c, Box{..}) = [(c', b) | (c', Just b) <- zip (subCubes c) subBoxes]

doQuery :: Collision q Cube Bool => q -> CubeBox a -> [a]
doQuery q !cb@(c, b)
  | not (collision q c) = []
  | otherwise = objects b ++ concatMap (doQuery q) (subs cb)

query :: Collision q Cube Bool => q -> CubeBox a -> [a]
query q (c, b) = doQuery q (c, b)
