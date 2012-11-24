{-# LANGUAGE UnicodeSyntax, RecordWildCards, TemplateHaskell, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

module TerrainGenerator (Cache, start, defaultConfig, sectorSize, sectorId, SectorId, cubeSize, sectors, totalBytes, bytesPerSector, sectorCenter, trianglesPerObstacle, verticesPerTriangle, bytesPerObstacle, verticesPerObstacle) where

import Data.Function (fix, on)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTVarIO, writeTChan, atomically, writeTVar, readTVarIO, newEmptyTMVarIO, tryTakeTMVar, putTMVar, takeTMVar, TChan, newTChanIO)
import Obstacles (randomObs)
import Graphics.Rendering.OpenGL.GL (GLdouble, Vector3(..), Color3(..))
import Math ((<+>), (<*>), V, VisualObstacle(..), GeometricObstacle(..), inner_prod, trianglesPerObstacle, verticesPerTriangle, StoredVertex, flatten)
import Data.Bits (xor)
import Control.Monad (replicateM, liftM3, foldM)
import Control.Monad.Random (evalRand, mkStdGen, getRandomR)
import Data.List (sortBy)
import MyUtil ((.))
import Prelude hiding ((.))
import Control.DeepSeq (deepseq, NFData(..))
import qualified Data.StorableVector as SV
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Foreign.C.Types (CFloat, CDouble)
import Foreign.Storable (Storable(..), sizeOf)
import Foreign.Storable.Tuple ()

cubeSize, bytesPerSector, bytesPerVertex, obstaclesPerSector, sectors, verticesPerSector, bytesPerDouble, bytesPerVector, bytesPerObstacle, bytesPerTriangle, verticesPerObstacle :: Num a ⇒ a
cubeSize = 7
sectors = cubeSize^3
obstaclesPerSector = 50
verticesPerObstacle = trianglesPerObstacle * verticesPerTriangle
bytesPerObstacle = trianglesPerObstacle * bytesPerTriangle
bytesPerTriangle = verticesPerTriangle * bytesPerVertex
verticesPerSector = verticesPerTriangle * trianglesPerObstacle * obstaclesPerSector
bytesPerVertex = fromIntegral $ sizeOf (undefined :: StoredVertex)
bytesPerDouble = fromIntegral $ sizeOf (undefined :: Double)
bytesPerVector = fromIntegral $ sizeOf (undefined :: Vector3 GLdouble)
bytesPerSector = bytesPerVertex * verticesPerSector
totalBytes = totalVertices * bytesPerVertex
totalVertices = verticesPerSector * sectors

data Config = Config { sectorSize :: GLdouble }

type SectorId = Vector3 Integer

data Sector = Sector
  { obstacles :: ![VisualObstacle]
  , sectorVertices :: !(SV.Vector StoredVertex) }

type Cache = Map SectorId Sector

instance NFData CFloat
instance NFData CDouble
instance NFData Sector

emptyCache :: Cache
emptyCache = Map.empty

sectorCenter :: Config → SectorId → V
sectorCenter Config{..} = (<*> sectorSize) . (fromInteger .)

seed :: SectorId → Int
seed (Vector3 x y z) = fromInteger $ xor x $ xor y z

buildSector :: Config → SectorId → Sector
buildSector config@Config{..} sid = Sector{..}
  where
    halfSectorSize = sectorSize / 2
    f = do
      obstacleColor ← getRandomR (Color3 0.3 0.3 0.3, Color3 1 1 1)
      replicateM obstaclesPerSector $ do
        c ← getRandomR (Vector3 (-halfSectorSize) (-halfSectorSize) (-halfSectorSize), Vector3 halfSectorSize halfSectorSize halfSectorSize)
        geometricObstacle ← randomObs (c <+> sectorCenter config sid) 800
        return VisualObstacle{..}
    obstacles = evalRand f $ mkStdGen $ seed sid
    sectorVertices = flatten obstacles

ball :: Set SectorId
ball = Set.fromAscList $
  sortBy (compare `on` (\v → inner_prod v v)) [v |  v ← liftM3 Vector3 cc cc cc, inner_prod v v <= 9]
cc :: [Integer]
cc = [0,1,-1,2,-2,3,-3,4,-4,5,-5,6,-6]

defaultConfig :: Config
defaultConfig = Config { sectorSize = 7000 {-, obstaclesPerSector = 50-} }

sectorId :: Config → V → SectorId
sectorId Config{..} (Vector3 x y z) = Vector3 (round (x / sectorSize)) (round (y / sectorSize)) (round (z / sectorSize))

distance :: SectorId → SectorId → Integer
distance (Vector3 x y z) (Vector3 x' y' z') = maximum $ abs . [x - x', y - y', z - z']

start :: Config → IO (TChan (SectorId, SV.Vector StoredVertex), V → IO [GeometricObstacle])
start config = do
  toBuffer ← newTChanIO
  queue ← newEmptyTMVarIO
  let initialCache = emptyCache
  mp ← newTVarIO initialCache
  forkIO $ flip fix initialCache $ \loop (storedSectors :: Cache) → do
    newSector ← atomically (sectorId config . takeTMVar queue)
    let sectorsWeWant = Set.map{-Monotonic-} (<+> newSector) ball
    foldM (\ss sid → do
      let y = buildSector config sid
      deepseq y $ do
      let n = Map.insert sid y ss
      atomically $ writeTChan toBuffer (sid, sectorVertices y) >> writeTVar mp n
      return n)
      (Map.filterWithKey (\k _ → k `Set.member` sectorsWeWant) storedSectors)
      (Set.toList (Set.difference sectorsWeWant (Map.keysSet storedSectors)))
        >>= loop
  return (toBuffer, \v → do
    atomically $ do
      tryTakeTMVar queue
      putTMVar queue v
    storedSectors ← readTVarIO mp
    let shootableObstacles = geometricObstacle . (filter (\(s, _) → distance s (sectorId config v) <= 1) (Map.toList storedSectors) >>= (obstacles . snd))
    return shootableObstacles)
