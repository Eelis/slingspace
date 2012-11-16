{-# LANGUAGE UnicodeSyntax, RecordWildCards #-}

module TerrainGenerator where

import Data.Function (fix)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTChanIO, newTVarIO, atomically, writeTChan, readTChan, writeTVar, readTVarIO)
import Obstacles (randomObs)
import Graphics.Rendering.OpenGL.GL hiding (Plane)
import Math ((<+>), V, VisualObstacle(..))
import Data.Bits (xor)
import Control.Monad (replicateM)
import Control.Monad.Random (evalRand, mkStdGen, getRandomR)
import MyUtil ((.))
import Prelude hiding ((.))

data WorldConfig = WorldConfig
  { sectorSize :: GLdouble
  , obstaclesPerSector :: Int }

type Sector = (Integer, Integer)

type TerrainCache = (Sector, [VisualObstacle])

emptyMap :: TerrainCache
emptyMap = ((-1, -1), [])

obstaclesAround :: WorldConfig → Sector → [VisualObstacle]
obstaclesAround WorldConfig{..} (x, z) = evalRand f $ mkStdGen $ fromInteger $ xor x z
  where
    halfSectorSize = sectorSize / 2
    f = do
      obstacleColor ← getRandomR (Color4 0.5 0.5 0.5 1, Color4 1 1 1 1)
      replicateM obstaclesPerSector $ do
        c ← getRandomR (Vector3 (-halfSectorSize) 500 (-halfSectorSize), Vector3 halfSectorSize 2000 halfSectorSize)
        geometricObstacle ← randomObs (c <+> Vector3 (fromInteger x * sectorSize) 0 (fromInteger z * sectorSize)) 800
        return VisualObstacle{..}

updateMap :: WorldConfig → Sector → TerrainCache → TerrainCache
updateMap worldConfig updatedPosition old@(oldPos, _)
  | updatedPosition /= oldPos = (updatedPosition, obstaclesAround worldConfig updatedPosition)
  | otherwise = old

defaultWorldConfig :: WorldConfig
defaultWorldConfig = WorldConfig { sectorSize = 10000, obstaclesPerSector = 30 }

sector :: WorldConfig → V → Sector
sector WorldConfig{..} (Vector3 x _ z) = (round (x / sectorSize), round (z / sectorSize))

startGenerator :: WorldConfig → IO (V → IO (), IO TerrainCache)
startGenerator config = do
  queue ← newTChanIO
  let initialMap = emptyMap
  mp ← newTVarIO initialMap
  forkIO $ flip fix initialMap $ \loop oldMap@(oldSector, _) → do
    newSector ← atomically (sector config . readTChan queue)
    if newSector /= oldSector
      then do
        let newMap = updateMap config newSector oldMap
        -- Todo: Fully eval newMap here.
        atomically $ writeTVar mp newMap
        loop newMap
      else loop oldMap
  return (atomically . writeTChan queue, readTVarIO mp)
