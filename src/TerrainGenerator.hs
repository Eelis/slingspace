{-# LANGUAGE UnicodeSyntax, RecordWildCards, TemplateHaskell #-}

module TerrainGenerator (Cache(..), start, defaultConfig, sectorSize, sectorId, SectorId, cubeSize, bytesPerVertex, totalVertices, sectors, totalBytes, bytesPerSector, doublesPerVector, bytesPerDouble, sectorCenter) where

import Data.Function (fix, on)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTVarIO, writeTChan, atomically, writeTVar, readTVarIO, newEmptyTMVarIO, tryTakeTMVar, putTMVar, takeTMVar, TChan, newTChanIO)
import Obstacles (randomObs)
import Graphics.Rendering.OpenGL.GL (GLdouble, GLfloat, Vector3(..), Color4(..))
import Math ((<+>), (<*>), V, VisualObstacle(..), GeometricObstacle(..), AnnotatedTriangle(..), inner_prod, Sphere(..))
import Data.Bits (xor)
import Control.Monad (replicateM, liftM3, forM, foldM)
import Control.Monad.Random (evalRand, mkStdGen, getRandomR)
import Data.List (sortBy)
import MyUtil ((.), tupleToList)
import Prelude hiding ((.))
import Control.DeepSeq (deepseq, NFData(..))
import Data.DeriveTH
import Data.Array.Storable
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)

cubeSize, bytesPerSector, bytesPerVertex, vectorsPerVertex, obstaclesPerSector, sectors, trianglesPerObstacle, verticesPerTriangle, verticesPerSector, doublesPerVector, bytesPerDouble :: Num a ⇒ a
cubeSize = 7
sectors = cubeSize^3
obstaclesPerSector = 50
trianglesPerObstacle = 4
verticesPerTriangle = 3
verticesPerSector = verticesPerTriangle * trianglesPerObstacle * obstaclesPerSector
vectorsPerVertex = 2
doublesPerVector = 3
bytesPerDouble = 8
bytesPerVertex = vectorsPerVertex * doublesPerVector * bytesPerDouble
bytesPerSector = bytesPerVertex * verticesPerSector
totalVertices = verticesPerSector * sectors
totalBytes = totalVertices * bytesPerVertex

data Config = Config
  { sectorSize :: GLdouble
  {-, obstaclesPerSector :: Int-} }

type SectorId = Vector3 Integer

data Sector = Sector
  { obstacles :: [VisualObstacle]
  , sectorVertices :: StorableArray Int GLdouble }

type Cache = Map SectorId Sector

instance NFData GLdouble
instance NFData GLfloat
instance NFData (StorableArray a b)

$( derive makeNFData ''Vector3 )
$( derive makeNFData ''Color4 )
$( derive makeNFData ''AnnotatedTriangle )
$( derive makeNFData ''GeometricObstacle )
$( derive makeNFData ''VisualObstacle )
$( derive makeNFData ''Sector )
$( derive makeNFData ''Sphere )

emptyCache :: Cache
emptyCache = Map.empty

vectorComponents :: Vector3 a → [a]
vectorComponents (Vector3 x y z) = [x, y, z]

listArray :: (MArray a e m) ⇒ [e] → m (a Int e)
listArray l = newListArray (0, length l - 1) l

sectorCenter :: Config → SectorId → V
sectorCenter Config{..} = (<*> sectorSize) . (fromInteger .)

seed :: SectorId → Int
seed (Vector3 x y z) = fromInteger $ xor x $ xor y z

buildSector :: Config → SectorId → IO Sector
buildSector config@Config{..} sid = do
  let
    halfSectorSize = sectorSize / 2
    f = do
      obstacleColor ← getRandomR (Color4 0.3 0.3 0.3 1, Color4 1 1 1 1)
      replicateM obstaclesPerSector $ do
        c ← getRandomR (Vector3 (-halfSectorSize) (-halfSectorSize) (-halfSectorSize), Vector3 halfSectorSize halfSectorSize halfSectorSize)
        geometricObstacle ← randomObs (c <+> sectorCenter config sid) 800
        return VisualObstacle{..}
    obstacles = evalRand f $ mkStdGen $ seed sid
  sectorVertices ← listArray $ concat
    [ vectorComponents vertex ++ vectorComponents triangleNormal
    | AnnotatedTriangle{..} ← geometricObstacle . obstacles >>= obstacleTriangles
    , vertex ← tupleToList triangleVertices ]
  return Sector{..}

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

start :: Config → IO (TChan (SectorId, StorableArray Int GLdouble), V → IO [GeometricObstacle])
start config = do
  toBuffer ← newTChanIO
  queue ← newEmptyTMVarIO
  let initialCache = emptyCache
  mp ← newTVarIO initialCache
  forkIO $ flip fix initialCache $ \loop storedSectors → do
    newSector ← atomically (sectorId config . takeTMVar queue)
    let sectorsWeWant = Set.map{-Monotonic-} (<+> newSector) ball
    foldM (\ss sid → do
      y ← buildSector config sid
      --deepseq y $ do
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
