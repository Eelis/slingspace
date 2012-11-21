{-# LANGUAGE RecordWildCards, UnicodeSyntax, ScopedTypeVariables #-}

import Gui (gui)
import qualified Gui
import Logic (Player(..), release, fire, tickPlayer)
import qualified Data.Map as Map
import Math (VisualObstacle(..), GeometricObstacle(..), Ray(..))
import Control.Monad.Fix (fix)
import MyGL ()
import MyUtil ((.), read_config_file)
import Graphics.UI.GLUT (Vector3(..), Color3(..))
import Obstacles (infinite_tunnel)
import Prelude hiding ((.))
import TupleProjection (project)
import Control.Monad.Random (evalRandIO)
import qualified TerrainGenerator

name :: String
name = "Player"

visualize :: GeometricObstacle -> VisualObstacle
visualize g = VisualObstacle g (Color3 0.9 0.9 0.9)

main :: IO ()
main = do
  tu_cfg ← read_config_file "infinite-tunnel.txt"
  gp_cfg ← read_config_file "gameplay.txt"

  gtunnel :: [GeometricObstacle] ← take 200 . ($(project 2) .) . evalRandIO (infinite_tunnel tu_cfg)

  print gtunnel

  let
    atunnel = TerrainGenerator.flatten (map visualize gtunnel)
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
      , tick = return (Just (0, atunnel), makeController (tail p))
      , release = \g -> makeController $ path $ release g $ head p
      , fire = \g v -> makeController $ path $ fire gp_cfg g v $ head p
      , spawn = self }

  gui (makeController $ path initialPlayer) name gp_cfg
