{-# LANGUAGE RecordWildCards #-}

import Gui (gui)
import qualified Gui
import Logic (Player(..), GraphNode(..), release, fire, tick_player)
import qualified Data.Map as Map
import Math (VisualObstacle(..), GeometricObstacle(..), Ray(..))
import Control.Monad.Fix (fix)
import MyGL ()
import MyUtil ((.), omni_map, read_config_file)
import Graphics.UI.GLUT (Vector3(..), Color4(..))
import Obstacles (infinite_tunnel)
import Prelude hiding ((.))
import TupleProjection (project)
import Control.Monad.Random (evalRandIO)

name :: String
name = "Player"

interleave :: [a] → [a] → [a]
interleave [] x = x
interleave x [] = x
interleave (h:t) (h':t') = h : h' : interleave t t'

omni_mapje :: [b] → (a → [b] → [b] → b) → [a] → [b]
omni_mapje _ _ [] = []
omni_mapje p f (h:t) = (h' : t')
  where
    t' = omni_mapje (h' : p) f t
    h' = f h t' p

to_graphnodes :: [GeometricObstacle] → [GraphNode]
to_graphnodes = omni_map GraphNode

myzip :: [a] → [a] → [a]
myzip [] x = x
myzip x [] = x
myzip (h:t) (h':t') = h:h':myzip t t'

visualize :: GeometricObstacle -> VisualObstacle
visualize g = VisualObstacle g (Color4 0.9 0.9 0.9 1)

main :: IO ()
main = do
  tu_cfg ← read_config_file "infinite-tunnel.txt"
  gp_cfg ← read_config_file "gameplay.txt"

  gtunnel :: [GeometricObstacle] ← take 200 . ($(project 2) .) . evalRandIO (infinite_tunnel tu_cfg)
  
  let
    tunnel = to_graphnodes gtunnel
    closest = head tunnel
    initialPosition = (Vector3 0 1800 1000)
    initialPlayer = Player (Ray initialPosition (Vector3 0 0 0)) Map.empty False
    path = iterate (tick_player gtunnel gp_cfg)

    makeState :: [Player] -> Gui.State
    makeState p = Gui.State
      { players = Map.singleton name p
      , shootableObstacles = gtunnel
      , visibleObstacles = map visualize gtunnel
      }

    makeController :: [Player] -> Gui.Controller
    makeController p = fix $ \self -> Gui.Controller
      { state = makeState p
      , tick = do
        return $ makeController (tail p)
      , release = \g -> return $ makeController $ path $ release g $ head p
      , fire = \g v -> return $ makeController $ path $ fire gp_cfg g v $ head p
      , spawn = return self }

  gui (makeController $ path initialPlayer) name gp_cfg
