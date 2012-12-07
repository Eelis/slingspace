{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module Controllers (Controller(..), BasicController(..), players) where

import Logic (Life, Gun, GameplayConfig, future)
import Obstacles (ObstacleTree)
import Math (V)
import Util (Any(Any))
import Data.Maybe (maybeToList)

class Controller c where
  player :: c → Maybe Life
  player = const Nothing
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

players :: Controller c => c → [Life]
players c = maybeToList (player c) ++ others c

instance Controller Life where
  player = Just
  tick = future


class Controller c => BasicController c where
  controllerObstacles :: c → ObstacleTree
  controllerGpCfg :: c → GameplayConfig
    -- assumed to remain constant

instance Controller (Any BasicController) where -- ghc /should/ be able to generate this
  player (Any c) = player c
  others (Any c) = others c
  tick (Any c) = Any (tick c)
  fire g v (Any c) = Any `fmap` fire g v c

instance BasicController (Any BasicController) where -- ghc /should/ be able to generate this
  controllerObstacles (Any c) = controllerObstacles c
  controllerGpCfg (Any c) = controllerGpCfg c
