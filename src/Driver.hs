{-# LANGUAGE RecordWildCards #-}

import Gui (gui, GuiCallbacks(..))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef, writeIORef)
import Logic (Player(..), GameplayConfig, release, fire, tick_player)
import qualified Data.Map as Map
import Math (Ray(..), V)
import MyGL ()
import MyUtil ((.), read_config_file)
import Graphics.UI.GLUT (Vector3(..))
import Prelude hiding ((.))
import TerrainGenerator (TerrainCache, startGenerator, defaultWorldConfig)

name :: String
name = "Player"

data State = State
  { player :: IORef Player
  , gameplayConfig :: GameplayConfig
  , informGenerator :: V → IO ()
  , getObstacles :: IO TerrainCache }

makeCallbacks :: State → GuiCallbacks
makeCallbacks State{..} = GuiCallbacks{..}
  where
    cc_tick = do
      (_, obs) ← getObstacles
      oldPlayer ← readIORef player
      let newPlayer = tick_player obs gameplayConfig oldPlayer
      informGenerator $ rayOrigin $ body newPlayer
      writeIORef player newPlayer
    cc_spawn = return ()
    cc_release g = modifyIORef player $ release g
    cc_fire g v = modifyIORef player $ fire gameplayConfig g v
    cc_players = Map.singleton name . (:[]) . readIORef player
    cc_visible_obstacles = snd . getObstacles
    cc_shootable_obstacles = snd . getObstacles

main :: IO ()
main = do
  gameplayConfig ← read_config_file "gameplay.txt"
  (informGenerator, getObstacles) ← startGenerator defaultWorldConfig
  let initialPosition = (Vector3 0 1800 0)
  informGenerator (Vector3 0 1800 0)
  player ← newIORef $ Player (Ray initialPosition (Vector3 0 0 0)) Map.empty False
  gui (makeCallbacks State{..}) name gameplayConfig
