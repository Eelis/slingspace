{-# LANGUAGE RecordWildCards, UnicodeSyntax, ScopedTypeVariables, NamedFieldPuns #-}

import Gui (gui)
import Logic (Player(..), fire, Life(..), safeFuture, live, gunConfig, GameplayConfig(..), birth)
import qualified Data.Map as Map
import Math (VisualObstacle(..), GeometricObstacle, Ray(..), asStoredVertices)
import Util ((.), read_config_file, loadConfig, getDataFileName, Any(Any))
import Graphics.Rendering.OpenGL.GL (Vector3(..))
import Obstacles (infinite_tunnel, bigCube, ObstacleTree)
import Prelude hiding ((.))
import Control.Monad.Random (evalRandIO)
import Guided (Guided(..))
import Controllers (Controller(..), BasicController(..))
import Stalker (Stalker(Stalker))
import qualified Recorder
import System.Random (mkStdGen)
import qualified Octree
import qualified SlingSpace.Configuration

trainingWheels, sideKick :: Bool
trainingWheels = False
sideKick = False

data C = C { life :: Life, obstacles :: ObstacleTree, gpCfg :: GameplayConfig }

instance BasicController C where
  controllerObstacles = obstacles
  controllerGpCfg = gpCfg

instance Controller C where
  player = Just . life
  tick c@C{..} = c{life=safeFuture (controllerObstacles c) (controllerGpCfg c) life}
  fire g v c@C{..} = do
    l ← live (controllerObstacles c) (controllerGpCfg c) . Logic.fire (gunConfig gpCfg g) g v . birth life
    return c{life=l}

main :: IO ()
main = do
  tuCfg ← read_config_file "infinite-tunnel.txt"
  gpCfg@GameplayConfig{gunConfig} ← getDataFileName "config/gameplay.hs" >>= loadConfig
  guiConfig ← getDataFileName "config/gui.hs" >>= loadConfig 

  obstacles :: [GeometricObstacle] ← take 2000 . ((\(_, _, x) → x) .) . evalRandIO (infinite_tunnel tuCfg)

  let
    tree = Octree.fromList bigCube obstacles
    vertices = asStoredVertices (map (VisualObstacle SlingSpace.Configuration.defaultObstacleColor) obstacles)
    initialPosition = Vector3 0 1800 (-2000)
    initialPlayer = Player (Ray initialPosition (Vector3 0 0 0)) Map.empty
    stalker = live tree gpCfg (Player (Ray (Vector3 0 1800 (-2000)) (Vector3 0 0 0)) Map.empty)

  r ← gui vertices tree guiConfig gunConfig pi $
    Recorder.record $
    (if trainingWheels then Any . Guided else id) $
    (if sideKick then Any . Stalker stalker (mkStdGen 3) else id) $
    (Any C{obstacles=tree, life=live tree gpCfg initialPlayer, ..} :: Any BasicController)

  putStrLn $ show (length (Recorder.frames r)) ++ " frames."
