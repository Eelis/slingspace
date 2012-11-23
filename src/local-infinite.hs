{-# LANGUAGE RecordWildCards, UnicodeSyntax, ScopedTypeVariables #-}

import Gui (gui)
import qualified Gui
import Logic (Player(..), release, fire, tickPlayer, GameplayConfig, Life(..), lifeAfter)
import qualified Data.Map as Map
import Math (VisualObstacle(..), GeometricObstacle(..), Ray(..), Cube(..), V)
import MyGL ()
import MyUtil ((.), read_config_file)
import Graphics.UI.GLUT (Vector3(..), Color3(..))
import Obstacles (infinite_tunnel)
import Prelude hiding ((.))
import Control.Monad.Random (evalRandIO)
import Control.DeepSeq (deepseq)
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

  let
    tree = Octree.fromList (Cube (Vector3 (-mega) (-mega) (-mega)) twomega) gtunnel
    atunnel = TerrainGenerator.flatten (map visualize gtunnel)
    initialPosition = Vector3 0 1800 (-2000)
    initialPlayer = Player (Ray initialPosition (Vector3 0 0 0)) Map.empty
    path = lifeAfter tree gp_cfg
    path' p = (p, path p)

    makeController :: (Player, Life) -> Gui.Controller
    makeController (p, l) = Gui.Controller
      { players = Map.singleton name (Life p l)
      , tick = return (Nothing, case l of
        Death v ->
          let phoenix = p{body=Ray v (Vector3 0 0 0)}
          in makeController (path' phoenix)
        Life p' l' -> makeController (p', l'))
      , release = \g -> makeController $ path' $ release g p
      , fire = \g v -> makeController $ path' $ fire gp_cfg g v p }

  deepseq tree $ do

  gui (makeController (path' initialPlayer)) (atunnel, tree) name gp_cfg
