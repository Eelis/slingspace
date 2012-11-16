{-# LANGUAGE RecordWildCards #-}

import Gui (gui, GuiCallback(..))
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

data LocalGuiCallback = LCC
  { player :: IORef Player
  , gameplayConfig :: GameplayConfig
  , informGenerator :: V → IO ()
  , getObstacles :: IO TerrainCache }

instance GuiCallback LocalGuiCallback where
  cc_tick LCC{..} = do
    (_, obs) ← getObstacles
    oldPlayer ← readIORef player
    let newPlayer = tick_player obs gameplayConfig oldPlayer
    informGenerator $ rayOrigin $ body newPlayer
    writeIORef player newPlayer
  cc_spawn _ = return ()
  cc_release LCC{..} g = modifyIORef player $ release g
  cc_fire LCC{..} g v = modifyIORef player $ fire gameplayConfig g v
  cc_players LCC{..} = Map.singleton name . (:[]) . readIORef player
  cc_visible_obstacles LCC{..} = snd . getObstacles
  cc_shootable_obstacles LCC{..} = snd . getObstacles

main :: IO ()
main = do
  gameplayConfig ← read_config_file "gameplay.txt"
  (informGenerator, getObstacles) ← startGenerator defaultWorldConfig
  let initialPosition = (Vector3 0 1800 0)
  informGenerator (Vector3 0 1800 0)
  player ← newIORef $ Player (Ray initialPosition (Vector3 0 0 0)) Map.empty False
  gui (LCC{..}) name gameplayConfig
