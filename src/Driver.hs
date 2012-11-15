{-# LANGUAGE RecordWildCards, UnicodeSyntax #-}

import Gui (gui)
import qualified Gui
import qualified Logic
import qualified Data.Map as Map
import Graphics.Rendering.OpenGL.GL (GLdouble)
import Logic (Player(..), GameplayConfig, tickPlayer, Gun)
import Math (Ray(..), V, GeometricObstacle)
import Data.Function (fix)
import MyGL ()
import MyUtil ((.), read_config_file)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (Vector3(..), ($=))
import Prelude hiding ((.))
import qualified TerrainGenerator
import TerrainGenerator (cubeSize)
import Data.Array.Storable (StorableArray, withStorableArray)
import Control.Concurrent.STM (STM, isEmptyTChan, atomically, TChan, readTChan)

name :: String
name = "Player"

data Static = Static
  { gameplayConfig :: GameplayConfig
  , toBuffer :: TChan (TerrainGenerator.SectorId, StorableArray Int GLdouble)
  , contactGenerator :: V → IO [GeometricObstacle] }

data State = State
  { player :: Player
  , shootableObstacles :: [GeometricObstacle] }

data Controller = Controller
  { state :: State
  , tick :: GLUT.BufferObject → IO Controller
  , release :: Gun → IO Controller
  , fire :: Gun → V → IO Controller
  , spawn :: IO Controller }

guiState :: State → Gui.State
guiState State{..} = Gui.State{ players = Map.singleton name [player], .. }

guiController :: Controller → Gui.Controller
guiController Controller{..} = fix $ \self → Gui.Controller
  { state = guiState state
  , tick = \b → guiController . tick b
  , release = \g → guiController . release g
  , fire = \g v → guiController . fire g v
  , spawn = return self }

type SectorId = Vector3 Integer

sectorIndex :: SectorId → Integer
sectorIndex (Vector3 x y z) = (m x * cubeSize + m y) * cubeSize + m z
  where m = (`mod` cubeSize)

maybeReadTChan :: TChan a → STM (Maybe a)
maybeReadTChan c = do
  b ← isEmptyTChan c
  if b then return Nothing else Just . readTChan c

control :: Static → State → Controller
control Static{..} = go where
  go state@State{..} = Controller
    { tick = \obstacleBuffer → do
        let
          newPlayer = tickPlayer shootableObstacles gameplayConfig player
          oldSector = TerrainGenerator.sectorId TerrainGenerator.defaultConfig $ rayOrigin $ body player
          newSector = TerrainGenerator.sectorId TerrainGenerator.defaultConfig $ rayOrigin $ body newPlayer
        if newSector == oldSector
          then do
            m ← atomically $ maybeReadTChan toBuffer
            case m of
              Nothing → return ()
              Just (sid, array) → do
                GLUT.bindBuffer GLUT.ArrayBuffer $= Just obstacleBuffer
                withStorableArray array $
                  GLUT.bufferSubData GLUT.ArrayBuffer GLUT.WriteToBuffer
                    (fromInteger $ sectorIndex sid * TerrainGenerator.bytesPerSector) TerrainGenerator.bytesPerSector
            return $ go $ state{player=newPlayer}
          else do
            newShootableObstacles ← contactGenerator $ rayOrigin $ body newPlayer
            return $ go $ State newPlayer newShootableObstacles
    , fire = \g v → return $ go state{player = Logic.fire gameplayConfig g v player}
    , release = \g → return $ go state{player = Logic.release g player}
    , spawn = return $ go state
    , .. }

main :: IO ()
main = do
  gameplayConfig ← read_config_file "gameplay.txt"
  (toBuffer, contactGenerator) ← TerrainGenerator.start TerrainGenerator.defaultConfig
  let initialPosition = (Vector3 0 20000 0)
  shootableObstacles ← contactGenerator initialPosition
  let player = Player (Ray initialPosition (Vector3 0 0 0)) Map.empty False
  gui (guiController $ control Static{..} State{..}) name gameplayConfig
