
module Main(main) where

import GLsling
import Data.IORef
import Control.Monad
import Logic
import qualified Data.Map as Map
import Math
import MyGL ()
import System.Random
import Graphics.UI.GLUT

data LocalClientController = LCC (IORef Player) [AnnotatedTriangle] LogicConfig

name = "Player"

instance ClientController LocalClientController where
  cc_tick (LCC pl o c) = modifyIORef pl $ tick_player c (Just o)
  cc_visible_obs (LCC _ o _) = return o
  cc_release (LCC p _ _) = modifyIORef p . release
  cc_fire (LCC p _ c) g v = modifyIORef p $ fire c g v
  cc_players (LCC p _ _) = fmap (Map.singleton name) $ readIORef p
  cc_find_target (LCC p o c) v w = fmap (\pl -> find_target o pl c v w) $ readIORef p

main = do
  logic_cfg <- liftM read $ readFile "gameplay-config.txt"
  obstacles <- liftM (fst . readRngMonad (niceTunnel logic_cfg)) getStdGen
  p <- newIORef $ Player (PlayerBody (Vector3 0 1800 1000) (Vector3 0 0 0)) Map.empty False -- dead
  gl_client (LCC p (concat $ map (\(Obstacle a b c d) -> [a, b, c, d]) obstacles) logic_cfg) name
