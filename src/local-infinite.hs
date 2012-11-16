{-# LANGUAGE RecordWildCards #-}

import Gui (gui)
import qualified Gui
import Logic (Player(..), release, fire, tickPlayer)
import qualified Data.Map as Map
import Math (VisualObstacle(..), GeometricObstacle(..), Ray(..))
import Control.Monad.Fix (fix)
import MyGL ()
import MyUtil ((.), read_config_file)
import Graphics.UI.GLUT (Vector3(..), Color4(..), ($=))
import qualified Graphics.UI.GLUT as GLUT
import Obstacles (infinite_tunnel)
import Prelude hiding ((.))
import TupleProjection (project)
import Control.Monad.Random (evalRandIO)
import Data.Array.Storable (withStorableArray)
import qualified TerrainGenerator

name :: String
name = "Player"

visualize :: GeometricObstacle -> VisualObstacle
visualize g = VisualObstacle g (Color4 0.9 0.9 0.9 1)

main :: IO ()
main = do
  tu_cfg ← read_config_file "infinite-tunnel.txt"
  gp_cfg ← read_config_file "gameplay.txt"

  gtunnel :: [GeometricObstacle] ← take 200 . ($(project 2) .) . evalRandIO (infinite_tunnel tu_cfg)

  let vtunnel = map visualize gtunnel
  atunnel <- TerrainGenerator.flatten vtunnel
  let
    initialPosition = (Vector3 0 1800 1000)
    initialPlayer = Player (Ray initialPosition (Vector3 0 0 0)) Map.empty False
    path = iterate (tickPlayer gtunnel gp_cfg)

    makeState :: [Player] -> Gui.State
    makeState p = Gui.State
      { players = Map.singleton name p
      , shootableObstacles = gtunnel }

    makeController :: [Player] -> Gui.Controller
    makeController p = fix $ \self -> Gui.Controller
      { state = makeState p
      , tick = \obstacleBuffer -> do
          GLUT.bindBuffer GLUT.ArrayBuffer $= Just obstacleBuffer
          withStorableArray atunnel $
            GLUT.bufferSubData GLUT.ArrayBuffer GLUT.WriteToBuffer 0 TerrainGenerator.bytesPerSector

          return $ makeController (tail p)
      , release = \g -> return $ makeController $ path $ release g $ head p
      , fire = \g v -> return $ makeController $ path $ fire gp_cfg g v $ head p
      , spawn = return self }

  gui (makeController $ path initialPlayer) name gp_cfg
