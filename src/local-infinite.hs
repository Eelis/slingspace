import Gui (gui, GuiCallback(..))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Logic (Player(..), GameplayConfig, GraphNode(..), release, fire, tick_player)
import qualified Data.Map as Map
import Math ((<->), AnnotatedObstacle(..), norm_2, Ray(..))
import MyGL ()
import MyUtil ((.), omni_map, read_config_file)
import Graphics.UI.GLUT (GLdouble, Vector3(..))
import Obstacles (infinite_tunnel)
import Prelude hiding ((.))
import TupleProjection (project)
import Control.Monad.Random (evalRandIO)

name :: String
name = "Player"

data LocalGuiCallback = LCC (IORef [Player]) GameplayConfig [AnnotatedObstacle]

instance GuiCallback LocalGuiCallback where
  cc_tick (LCC r _ _) = modifyIORef r tail
  cc_spawn (LCC _ _ _) = return ()
  cc_release (LCC p c a) g = modifyIORef p $ iterate (tick_player a c) . release g . head
  cc_fire (LCC p c a) g v = modifyIORef p $ iterate (tick_player a c) . fire c g v . head
  cc_players (LCC p _ _) = Map.singleton name . readIORef p
  cc_visible_obstacles (LCC _ _ t) = return t
  cc_shootable_obstacles (LCC _ _ t) = return t

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

to_graphnodes :: [AnnotatedObstacle] → [GraphNode]
to_graphnodes = omni_map GraphNode

myzip :: [a] → [a] → [a]
myzip [] x = x
myzip x [] = x
myzip (h:t) (h':t') = h:h':myzip t t'

{-to_graphnodes :: [AnnotatedObstacle] → [GraphNode]
to_graphnodes os = f os []
  where
    f :: [AnnotatedObstacle] → [GraphNode] → [GraphNode]
    f [] _ = []
    f (h:t) prev = h' : t'
      where
        t' :: [GraphNode]
        t' = f t (h' : prev)
        h' :: GraphNode
        h' = GraphNode h (myzip t' prev)-}
      

-- to_graphnodes = omni_mapje [] (\ao before after →
--   let x = unsafePerformIO (putStrLn "dus.." >> return 3)
--   in if x == 0 then undefined else GraphNode ao (interleave before after))

--to_graphnodes = omni_mapje [] (\ao before after → GraphNode ao (interleave before after))

--to_graphnodes = omni_map (\ao gns → GraphNode ao $ take 10 gns)

main :: IO ()
main = do
  tu_cfg ← read_config_file "infinite-tunnel.txt"
  gp_cfg ← read_config_file "gameplay.txt"

  --t ← fst . readRngMonad (infinite_tunnel tu_cfg) . getStdGen
  --putStr $ unlines $ take 100 (show . $(project 0) . t)

  atunnel :: [AnnotatedObstacle] ← take 200 . ($(project 2) .) . evalRandIO (infinite_tunnel tu_cfg)
  let tunnel = to_graphnodes atunnel
  --print $ length $ take 100000 bla
  let closest = head tunnel
  p ← newIORef $ iterate (tick_player atunnel gp_cfg) $ Player (Ray (Vector3 0 1800 1000) (Vector3 0 0 0)) Map.empty False
  gui (LCC p gp_cfg atunnel) name gp_cfg
