{-# LANGUAGE RecordWildCards, UnicodeSyntax, ScopedTypeVariables #-}

import Gui (gui)
import qualified Gui
import Logic (Player(..), release, fire, tickPlayer)
import qualified Data.Map as Map
import Math (VisualObstacle(..), GeometricObstacle(..), Ray(..), Cube(..))
import MyGL ()
import MyUtil ((.), read_config_file)
import Graphics.UI.GLUT (Vector3(..), Color3(..))
import Obstacles (infinite_tunnel)
import Prelude hiding ((.))
import Control.Monad.Random (evalRandIO)
import qualified TerrainGenerator
import qualified Octree

name :: String
name = "Player"

visualize :: GeometricObstacle -> VisualObstacle
visualize g = VisualObstacle g (Color3 0.9 0.9 0.9)

mega = 2^20
twomega = 2^21
c = (Cube (Vector3 (-mega) (-mega) (-mega)) twomega)

main :: IO ()
main = do
  tu_cfg ← read_config_file "infinite-tunnel.txt"
  gp_cfg ← read_config_file "gameplay.txt"

  gtunnel :: [GeometricObstacle] ← take 1000 . ((\(_, _, x) -> x) .) . evalRandIO (infinite_tunnel tu_cfg)

  --print (length (show gtunnel))
  let tree = Octree.fromList (Cube (Vector3 (-mega) (-mega) (-mega)) twomega) gtunnel
  --print $ snd tree

  let
    atunnel = TerrainGenerator.flatten (map visualize gtunnel)
    initialPosition = Vector3 0 1800 (-2000)
    initialPlayer = Player (Ray initialPosition (Vector3 0 0 0)) Map.empty False
    path = iterate (tickPlayer tree gp_cfg)

    makeController :: [Player] -> Gui.Controller
    makeController p = Gui.Controller
      { players = Map.singleton name p
      , tick = return (Nothing, makeController $ tail p)
      , release = \g -> makeController $ path $ release g $ head p
      , fire = \g v -> makeController $ path $ fire gp_cfg g v $ head p }

  gui (makeController $ path initialPlayer) (atunnel, tree) name gp_cfg
