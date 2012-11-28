{-# LANGUAGE RecordWildCards, UnicodeSyntax, ScopedTypeVariables #-}

import Gui (gui)
import qualified Gui
import Logic (Player(..), release, fire, Life(..), lifeAfter, lifeExpectancyUpto, shooting_range, immortalize, live)
import qualified Data.Map as Map
import Math (VisualObstacle(..), GeometricObstacle, Ray(..), asStoredVertices)
import MyGL ()
import MyUtil ((.), read_config_file)
import Graphics.UI.GLUT (Vector3(..), Color3(..))
import Obstacles (infinite_tunnel, bigCube)
import Prelude hiding ((.))
import Control.Monad.Random (evalRandIO)
import Data.Function (on)
import qualified Octree

name :: String
name = "Player"

betterThan :: Life → Life → Bool
betterThan = (>=) `on` lifeExpectancyUpto 300

trainingWheels :: Bool
trainingWheels = False

main :: IO ()
main = do
  tu_cfg ← read_config_file "infinite-tunnel.txt"
  gp_cfg ← read_config_file "gameplay.txt"

  obstacles :: [GeometricObstacle] ← take 1000 . ((\(_, _, x) -> x) .) . evalRandIO (infinite_tunnel tu_cfg)

  let
    tree = Octree.fromList bigCube obstacles
    vertices = asStoredVertices (map (VisualObstacle (Color3 0.9 0.9 0.9)) obstacles)
    initialPosition = Vector3 0 1800 (-2000)
    initialPlayer = Player (Ray initialPosition (Vector3 0 0 0)) Map.empty

    makeController :: Life -> Gui.Controller
    makeController l@(Life p f) = Gui.Controller
      { players = Map.singleton name l
      , tick = return (Nothing, makeController f)
      , release = \g -> consider (release g p)
      , fire = \g v -> consider (fire gp_cfg g v p) }
      where
        consider new = if not trainingWheels || (l' `betterThan` l)
            then Just $ makeController (Life new (immortalize tree gp_cfg l'))
            else Nothing
          where l' = lifeAfter tree gp_cfg new

  gui
    (makeController (immortalize tree gp_cfg $ live tree gp_cfg initialPlayer))
    (vertices, tree)
    name
    (shooting_range gp_cfg)
