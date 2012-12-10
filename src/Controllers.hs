{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module Controllers (Controller(..), BasicController(..), players) where

import Logic (Life, Gun, SimulationConfig, future)
import Obstacles (ObstacleTree)
import Math (V)
import Util (Any(Any))
import Control.Applicative (liftA2)

class Controller c where
  player :: c → Life
  others :: c → [Life]
  others = const []
  tick :: c → c
  tick = id
  fire :: Gun → Maybe V → c → Maybe c -- a Nothing V means release
  fire _ _ _ = Nothing
  -- fire returns Maybes so that the controller can say "nope, you can't fire/release now"

instance Controller (Any Controller) where -- ghc /should/ be able to generate this
  player (Any c) = player c
  others (Any c) = others c
  tick (Any c) = Any (tick c)
  fire g v (Any c) = Any `fmap` fire g v c

players :: Controller c ⇒ c → [Life]
players = liftA2 (:) player others

instance Controller Life where
  player = id
  tick = future

class Controller c ⇒ BasicController c where
  controllerObstacles :: c → ObstacleTree
  controllerConfig :: c → SimulationConfig
    -- assumed to remain constant

instance Controller (Any BasicController) where -- ghc /should/ be able to generate this
  player (Any c) = player c
  others (Any c) = others c
  tick (Any c) = Any (tick c)
  fire g v (Any c) = Any `fmap` fire g v c

instance BasicController (Any BasicController) where -- ghc /should/ be able to generate this
  controllerObstacles (Any c) = controllerObstacles c
  controllerConfig (Any c) = controllerConfig c
