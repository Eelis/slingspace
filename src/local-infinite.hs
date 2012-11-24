{-# LANGUAGE RecordWildCards, UnicodeSyntax, ScopedTypeVariables #-}

import Gui (gui)
import qualified Gui
import Logic (Player(..), release, fire, Life(..), lifeAfter, lifeExpectancyUpto)
import qualified Data.Map as Map
import Math (VisualObstacle(..), GeometricObstacle(..), Ray(..), Cube(..), flatten)
import MyGL ()
import MyUtil ((.), read_config_file)
import Graphics.UI.GLUT (Vector3(..), Color3(..))
import Obstacles (infinite_tunnel)
import Prelude hiding ((.))
import Control.Monad.Random (evalRandIO)
import Data.Function (on)
import Control.DeepSeq (deepseq)
import qualified Octree

name :: String
name = "Player"

visualize :: GeometricObstacle -> VisualObstacle
visualize g = VisualObstacle g (Color3 0.9 0.9 0.9)

mega = 2^20
twomega = 2^21

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
    tree = Octree.fromList (Cube (Vector3 (-mega) (-mega) (-mega)) twomega) obstacles
    vertices = flatten (map visualize obstacles)
    initialPosition = Vector3 0 1800 (-2000)
    initialPlayer = Player (Ray initialPosition (Vector3 0 0 0)) Map.empty
    consider l new = if not trainingWheels || (l' `betterThan` l) then Just $ makeController (new, l') else Nothing
      where l' = lifeAfter tree gp_cfg new

    makeController :: (Player, Life) -> Gui.Controller
    makeController (p, l) = Gui.Controller
      { players = Map.singleton name (Life p l)
      , tick = return (Nothing, case l of
        Death v ->
          let phoenix = p{body=Ray v (Vector3 0 0 0)}
          in makeController (phoenix, lifeAfter tree gp_cfg phoenix)
        Life p' l' -> makeController (p', l'))
      , release = \g -> consider l (release g p)
      , fire = \g v -> consider l (fire gp_cfg g v p) }

  deepseq tree $ do

  gui (makeController (initialPlayer, lifeAfter tree gp_cfg initialPlayer)) (vertices, tree) name gp_cfg
