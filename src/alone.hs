{-# LANGUAGE RecordWildCards, UnicodeSyntax, ScopedTypeVariables, NamedFieldPuns, ViewPatterns #-}

import Gui (gui)
import Logic (Player(..), fire, Life(..), safeFuture, live, gunConfig, GameplayConfig(..), birth, SimulationConfig(..))
import qualified Data.Map as Map
import Math (VisualObstacle(..), GeometricObstacle, Ray(..), randomAngle, unitCirclePoint)
import Data.AdditiveGroup ((^+^))
import Data.VectorSpace ((^*))
import Util ((.), loadConfig, getDataFileName, Any(Any))
import Graphics.Rendering.OpenGL.GL (Vector3(..))
import Obstacles (ObstacleTree, grow, randomObs)
import Prelude hiding ((.))
import Control.Monad (replicateM, liftM2)
import Control.Monad.Random (evalRandIO, MonadRandom(..))
import Guided (Guided(..))
import Controllers (Controller(..), BasicController(..))
import Stalker (Stalker(Stalker))
-- import qualified Recorder
import System.Random (mkStdGen)
import qualified SlingSpace.Configuration

trainingWheels, sideKick :: Bool
trainingWheels = False
sideKick = False

data C = C { life :: Life, obstacles :: ObstacleTree, simCfg :: SimulationConfig }

instance BasicController C where
  controllerObstacles = obstacles
  controllerConfig = simCfg

instance Controller C where
  player = life
  tick c@C{..} = c{life=safeFuture (controllerObstacles c) (controllerConfig c) life}
  onChar _ ' ' = error "exit"
  onChar _ _ = Nothing
  fire g v c@C{..} = do
    l ← live (controllerObstacles c) (controllerConfig c) . Logic.fire simCfg g v . birth life
    return c{life=l}

rawObstacles :: (Functor m, MonadRandom m) ⇒ m [GeometricObstacle]
rawObstacles = replicateM 600 $
  liftM2 (^+^)
    ((^* 30000) . unitCirclePoint . randomAngle)
    (getRandomR (Vector3 (-1200) 0 (-1200), Vector3 1000 2000 1000))
      >>= randomObs 600

main :: IO ()
main = do
  gpCfg@GameplayConfig{gunConfig} ← getDataFileName "config/gameplay.hs" >>= loadConfig
  guiConfig ← getDataFileName "config/gui.hs" >>= loadConfig 

  (tree, obstacles) ← grow . evalRandIO rawObstacles

  putStrLn $ show (length obstacles) ++ " obstacles."

  let
    vobstacles = map (VisualObstacle SlingSpace.Configuration.defaultObstacleColor) obstacles
    initialPosition = Vector3 0 1800 30000
    initialPlayer = Player (Ray initialPosition (Vector3 0 0 0)) Map.empty

  {-r ←-}
  gui vobstacles tree guiConfig gunConfig 0 $ \(SimulationConfig gpCfg → simCfg) →
    let
      stalker = live tree simCfg (Player (Ray (Vector3 0 1800 (-2000)) (Vector3 0 0 0)) Map.empty)
    in
      --Recorder.record $
      (if trainingWheels then Any . Guided else id) $
      (if sideKick then Any . Stalker stalker (mkStdGen 3) else id)
      (Any C{obstacles=tree, life=live tree simCfg initialPlayer, ..} :: Any BasicController)

  --putStrLn $ show (length (Recorder.frames r)) ++ " frames."
  return ()
