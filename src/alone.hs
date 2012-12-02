{-# LANGUAGE RecordWildCards, UnicodeSyntax, ScopedTypeVariables, NamedFieldPuns, ViewPatterns #-}

import Gui (gui)
import qualified Gui
import Logic (Player(..), release, fire, Life(..), lifeExpectancyUpto, immortalize, live, gunConfig, GameplayConfig(..))
import qualified Data.Map as Map
import Math (VisualObstacle(..), GeometricObstacle, Ray(..), asStoredVertices)
import Util ((.), read_config_file, loadConfig, getDataFileName)
import Graphics.Rendering.OpenGL.GL (Vector3(..))
import Obstacles (infinite_tunnel, bigCube)
import Prelude hiding ((.))
import Control.Monad.Random (evalRandIO)
import Data.Function (on)
import qualified Octree
import qualified SlingSpace.Configuration

name :: String
name = "Player"

betterThan :: Life → Life → Bool
betterThan = (>=) `on` lifeExpectancyUpto 300

trainingWheels :: Bool
trainingWheels = False

main :: IO ()
main = do
  tuCfg ← read_config_file "infinite-tunnel.txt"
  gpCfg@GameplayConfig{gunConfig} ← getDataFileName "config/gameplay.hs" >>= loadConfig
  guiConfig ← getDataFileName "config/gui.hs" >>= loadConfig 

  obstacles :: [GeometricObstacle] ← take 2000 . ((\(_, _, x) -> x) .) . evalRandIO (infinite_tunnel tuCfg)

  let
    tree = Octree.fromList bigCube obstacles
    vertices = asStoredVertices (map (VisualObstacle SlingSpace.Configuration.defaultObstacleColor) obstacles)
    initialPosition = Vector3 0 1800 (-2000)
    initialPlayer = Player (Ray initialPosition (Vector3 0 0 0)) Map.empty

    makeController :: Life -> Gui.Controller
    makeController l@(Life p f) = Gui.Controller
      { players = Map.singleton name l
      , tick = makeController f
      , release = \g -> consider (release g p)
      , fire = \g v -> consider (fire (gunConfig g) g v p) }
      where
        consider (live tree gpCfg → l')
          | not trainingWheels || (l' `betterThan` l) = Just $ makeController $ immortalize tree gpCfg l'
          | otherwise = Nothing

  gui
    (makeController (immortalize tree gpCfg $ live tree gpCfg initialPlayer))
    vertices
    tree
    name
    guiConfig
    gunConfig
    SlingSpace.Configuration.def
