{-# LANGUAGE RecordWildCards #-}

import Gui (gui)
import qualified Gui
import qualified Logic
import qualified Data.Map as Map
import Logic (Player(..), GameplayConfig, tick_player, Gun)
import Math (Ray(..), V, VisualObstacle(..))
import Data.Function (fix)
import MyGL ()
import MyUtil ((.), read_config_file)
import Graphics.UI.GLUT (Vector3(..))
import Prelude hiding ((.))
import TerrainGenerator (TerrainCache, startGenerator, defaultWorldConfig)

name :: String
name = "Player"

data Static = Static
  { gameplayConfig :: GameplayConfig
  , informGenerator :: V → IO ()
  , getObstacles :: IO TerrainCache }

data State = State
  { player :: Player
  , obstacles :: [VisualObstacle] }

data Controller = Controller
  { state :: State
  , tick :: IO Controller
  , release :: Gun → IO Controller
  , fire :: Gun → V → IO Controller
  , spawn :: IO Controller }

guiState :: State → Gui.State
guiState State{..} = Gui.State
  { players = Map.singleton name [player]
  , shootableObstacles = geometricObstacle . obstacles
  , visibleObstacles = obstacles }

guiController :: Controller → Gui.Controller
guiController Controller{..} = fix $ \self → Gui.Controller
  { state = guiState state
  , tick = guiController . tick
  , release = \g → guiController . release g
  , fire = \g v → guiController . fire g v
  , spawn = return self }

control :: Static → State → Controller
control Static{..} = go where
  go state@State{..} = Controller
    { tick = do
        let newPlayer = tick_player (geometricObstacle . obstacles) gameplayConfig player
        informGenerator $ rayOrigin $ body newPlayer
        newObs ← snd . getObstacles
        return $ go $ State newPlayer newObs
    , fire = \g v → return $ go state{player = Logic.fire gameplayConfig g v player}
    , release = \g → return $ go state{player = Logic.release g player}
    , spawn = return $ go state, .. }

main :: IO ()
main = do
  gameplayConfig ← read_config_file "gameplay.txt"
  (informGenerator, getObstacles) ← startGenerator defaultWorldConfig
  let
    initialPosition = (Vector3 0 1800 0)
    initialState = State (Player (Ray initialPosition (Vector3 0 0 0)) Map.empty False) []
  informGenerator initialPosition
  gui (guiController $ control Static{..} initialState) name gameplayConfig
