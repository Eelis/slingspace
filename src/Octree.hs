{-# LANGUAGE RecordWildCards, NamedFieldPuns, FlexibleContexts, PatternGuards #-}

module Octree (Box, CubeBox, empty, query, insert, toList, fromList) where

import Graphics.Rendering.OpenGL.GL (Vector3(..))
import Math (Collision(collision), FitsInCube(fitsInCube), Cube(..), (<*>), (<+>))
import MyUtil (orElse)

-- import Debug.Trace (trace)

type Subs a = [Maybe (Box a)]

subIds :: [Vector3 Int]
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
  , subs :: !(Subs a) }


showBox :: Show a => Box a -> [String]
showBox Box{..} =
  ("[" ++ replicate (length objects) '+' {-show objects-} ++ "]") : map ("  " ++)
    (concatMap (\mb -> case mb of Nothing -> []; Just b -> showBox b) subs)


instance Show a => Show (Box a) where
  show = unlines . showBox


type CubeBox a = (Cube, Box a)


boxToList :: Box a -> [a]
boxToList Box{..} = objects ++ concatMap (\mb -> case mb of Nothing -> []; Just b -> boxToList b) subs


toList :: CubeBox a -> [a]
toList = boxToList . snd

emptySubs :: Subs a
emptySubs = replicate (length subIds) Nothing

emptyBox :: Box a
emptyBox = Box [] emptySubs

empty :: Cube -> CubeBox a
empty c = (c, emptyBox)


subCube :: Cube -> Vector3 Int -> Cube
subCube Cube{..} i = Cube (cubeCorner <+> (i <*> subOff)) subSize
  where
    subOff = cubeSize `div` 3
    subSize = subOff * 2

subCubes :: Cube -> [Cube]
subCubes cube = map (subCube cube) subIds

doInsert :: FitsInCube a => a -> Cube -> Maybe (Box a) -> Maybe (Box a)
doInsert obj cube mb
  | not (obj `fitsInCube` cube) = Nothing
  | otherwise = Just $ case mapFirst (zip (subCubes cube) subs) f of
        Nothing -> Box (obj : objects) subs
        Just subs' -> Box objects (map snd subs')
  where
    Box{objects,subs} = mb `orElse` emptyBox
    f (c', msb) = case doInsert obj c' msb of
      Nothing -> Nothing
      Just x -> Just (c', Just x)

insert :: (Show a, FitsInCube a) => CubeBox a -> a -> CubeBox a
insert (c, b) o
  | Just b' <- doInsert o c (Just b) = (c, b')
  | otherwise = error $ "object " ++ show o ++ " did not fit in cube " ++ show c

fromList :: (Show a, FitsInCube a) => Cube -> [a] -> CubeBox a
fromList c = foldl insert (empty c)

doQuery :: Collision q Cube Bool => q -> Cube -> Box a -> [a]
doQuery q cube@Cube{..} Box{..}
  | not (collision q cube) = []
  | otherwise = objects ++ concatMap f (zip (subCubes cube) subs)
  where
    f (_, Nothing) = []
    f (c, Just b) = doQuery q c b

query :: Collision q Cube Bool => q -> CubeBox a -> [a]
query q (c, b) = doQuery q c b
