{-# LANGUAGE UnicodeSyntax, RecordWildCards #-}

module TerrainGenerator (Cache(..), start, defaultConfig) where

import Data.Function (fix)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTChanIO, newTVarIO, atomically, writeTChan, readTChan, writeTVar, readTVarIO)
import Obstacles (randomObs)
import Graphics.Rendering.OpenGL.GL hiding (Plane)
import Math ((<+>), V, VisualObstacle(..), GeometricObstacle)
import Data.Bits (xor)
import Control.Monad (replicateM)
import Control.Monad.Random (evalRand, mkStdGen, getRandomR)
import Data.List ((\\))
import MyUtil ((.))
import Prelude hiding ((.))

data Config = Config
  { sectorSize :: GLdouble
  , obstaclesPerSector :: Int }

type Sector = (Integer, Integer)

data Cache = Cache
  { currentSector :: Sector
  , storedSectors :: [(Sector, [VisualObstacle])] }

emptyCache :: Cache
emptyCache = Cache{currentSector = (-1, -1), storedSectors = []}

obstaclesAround :: Config → Sector → [VisualObstacle]
obstaclesAround Config{..} (x, z) = evalRand f $ mkStdGen $ fromInteger $ xor x z
  where
    halfSectorSize = sectorSize / 2
    f = do
      obstacleColor ← getRandomR (Color4 0.5 0.5 0.5 1, Color4 1 1 1 1)
      replicateM obstaclesPerSector $ do
        c ← getRandomR (Vector3 (-halfSectorSize) 500 (-halfSectorSize), Vector3 halfSectorSize 2000 halfSectorSize)
        geometricObstacle ← randomObs (c <+> Vector3 (fromInteger x * sectorSize) 0 (fromInteger z * sectorSize)) 800
        return VisualObstacle{..}

updateMap :: Config → Sector → Cache → Cache
updateMap config newSector@(x, z) oldCache@Cache{..}
  | newSector == currentSector = oldCache
  | otherwise = Cache
    { currentSector = newSector
    , storedSectors = filter ((`elem` sectorsWeWant) . fst) storedSectors
        ++ map (\s → (s, obstaclesAround config s)) (sectorsWeWant \\ fst . storedSectors) }
  where
    sectorsWeWant = [(x', z') | x' ← [x - 3 .. x + 3], z' ← [z - 3 .. z + 3]]

defaultConfig :: Config
defaultConfig = Config { sectorSize = 10000, obstaclesPerSector = 30 }

sector :: Config → V → Sector
sector Config{..} (Vector3 x _ z) = (round (x / sectorSize), round (z / sectorSize))

distance :: Sector → Sector → Integer
distance (x, z) (x', z') = max (x - x') (z - z')

start :: Config → IO (V → IO ([GeometricObstacle], [VisualObstacle]))
start config = do
  queue ← newTChanIO
  let initialCache = emptyCache
  mp ← newTVarIO initialCache
  forkIO $ flip fix initialCache $ \loop oldMap@Cache{..} → do
    newSector ← atomically (sector config . readTChan queue)
    if newSector /= currentSector
      then do
        let newMap = updateMap config newSector oldMap
        -- Todo: Fully eval newMap here.
        atomically $ writeTVar mp newMap
        loop newMap
      else loop oldMap
  return $ \v → do
    atomically $ writeTChan queue v
    Cache{..} ← readTVarIO mp
    let shootableObstacles = geometricObstacle . (filter (\(s, _) → distance s currentSector <= 1) storedSectors >>= snd)
    return (shootableObstacles, storedSectors >>= snd)
