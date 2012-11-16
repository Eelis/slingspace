{-# LANGUAGE UnicodeSyntax, RecordWildCards, TemplateHaskell #-}

module TerrainGenerator (Cache(..), start, defaultConfig, flatten) where

import Data.Function (fix)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTVarIO, atomically, writeTVar, readTVarIO, newEmptyTMVarIO, tryTakeTMVar, putTMVar, takeTMVar)
import Obstacles (randomObs)
import Graphics.Rendering.OpenGL.GL (GLdouble, GLfloat, Vector3(..), Color4(..))
import Math ((<+>), V, VisualObstacle(..), GeometricObstacle(..), AnnotatedTriangle(..))
import Data.Bits (xor)
import Control.Monad (replicateM)
import Control.Monad.Random (evalRand, mkStdGen, getRandomR)
import Data.List ((\\))
import MyUtil ((.), tupleToList)
import Prelude hiding ((.))
import Control.DeepSeq (deepseq, NFData(..))
import Data.DeriveTH
import Data.Array.Storable

data Config = Config
  { sectorSize :: GLdouble
  , obstaclesPerSector :: Int }

type SectorId = Vector3 Integer

data Sector = Sector
  { obstacles :: [VisualObstacle]
  , sectorVertices :: StorableArray Int GLdouble }

data Cache = Cache
  { currentSector :: SectorId
  , storedSectors :: [(SectorId, Sector)] }

instance NFData GLdouble
instance NFData GLfloat
instance NFData (StorableArray a b)

$( derive makeNFData ''Vector3 )
$( derive makeNFData ''Color4 )
$( derive makeNFData ''AnnotatedTriangle )
$( derive makeNFData ''GeometricObstacle )
$( derive makeNFData ''VisualObstacle )
$( derive makeNFData ''Sector )
$( derive makeNFData ''Cache )

emptyCache :: Cache
emptyCache = Cache{currentSector = Vector3 (-1) (-1) (-1), storedSectors = []}

vectorComponents :: Vector3 a → [a]
vectorComponents (Vector3 x y z) = [x, y, z]

listArray :: (MArray a e m) ⇒ [e] → m (a Int e)
listArray l = newListArray (0, length l - 1) l

flatten :: [VisualObstacle] → IO (StorableArray Int GLdouble)
flatten obstacles = listArray $ concat
  [ vectorComponents vertex ++ vectorComponents triangleNormal
  | AnnotatedTriangle{..} ← geometricObstacle . obstacles >>= obstacleTriangles
  , vertex ← tupleToList triangleVertices ]

buildSector :: Config → SectorId → IO Sector
buildSector Config{..} (Vector3 x y z) = do
  let
    halfSectorSize = sectorSize / 2
    f = do
      obstacleColor ← getRandomR (Color4 0.3 0.3 0.3 1, Color4 1 1 1 1)
      replicateM obstaclesPerSector $ do
        c ← getRandomR (Vector3 (-halfSectorSize) (-halfSectorSize) (-halfSectorSize), Vector3 halfSectorSize halfSectorSize halfSectorSize)
        geometricObstacle ← randomObs (c <+> Vector3 (fromInteger x * sectorSize) (fromInteger y * sectorSize) (fromInteger z * sectorSize)) 800
        return VisualObstacle{..}
    obstacles :: [VisualObstacle] = evalRand f $ mkStdGen $ fromInteger $ xor x $ xor y z
  sectorVertices ← flatten obstacles
  return Sector{..}

updateMap :: Config → SectorId → Cache → IO Cache
updateMap config newSector@(Vector3 x y z) oldCache@Cache{..}
  | newSector == currentSector = return oldCache
  | otherwise = do
    n ← mapM (\s → do sec ← buildSector config s; return (s, sec)) sectorsToAdd
    return $ Cache
      { currentSector = newSector
      , storedSectors = filter ((`elem` sectorsWeWant) . fst) storedSectors ++ n }
  where
    sectorsWeWant = [Vector3 x' y' z' | x' ← [x - 3 .. x + 3], y' ← [y - 3 .. y + 3], z' ← [z - 3 .. z + 3]]
    sectorsToAdd = sectorsWeWant \\ fst . storedSectors

defaultConfig :: Config
defaultConfig = Config { sectorSize = 3000, obstaclesPerSector = 8 }

sectorId :: Config → V → SectorId
sectorId Config{..} (Vector3 x y z) = Vector3 (round (x / sectorSize)) (round (y / sectorSize)) (round (z / sectorSize))

distance :: SectorId → SectorId → Integer
distance (Vector3 x y z) (Vector3 x' y' z') = maximum $ abs . [x - x', y - y', z - z']

start :: Config → IO (V → IO ([GeometricObstacle], [StorableArray Int GLdouble]))
start config = do
  queue ← newEmptyTMVarIO
  let initialCache = emptyCache
  mp ← newTVarIO initialCache
  forkIO $ flip fix initialCache $ \loop oldMap@Cache{..} → do
    newSector ← atomically (sectorId config . takeTMVar queue)
    if newSector /= currentSector
      then do
        newMap ← updateMap config newSector oldMap
        putStrLn "starting deepseq..."
        deepseq newMap $ do
        putStrLn "seq'd, committing..."
        atomically $ writeTVar mp newMap
        putStrLn "committed, looping..."
        loop newMap
      else loop oldMap
  return $ \v → do
    atomically $ do
      tryTakeTMVar queue
      putTMVar queue v
    Cache{..} ← readTVarIO mp
    let shootableObstacles = geometricObstacle . (filter (\(s, _) → distance s currentSector <= 1) storedSectors >>= (obstacles . snd))
    return (shootableObstacles, sectorVertices . snd . storedSectors)
