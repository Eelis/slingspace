{-# LANGUAGE UnicodeSyntax, RecordWildCards #-}

module Stalker (Stalker(..)) where

import Graphics.Rendering.OpenGL.GL (GLdouble, Vector3(..))
import Math (V)
import Logic (Life, lifeExpectancyUpto, positions, tryRandomAction, future, reviseIfWise)
import Controllers (Controller(..), BasicController(..))
import Util (average)
import Control.Monad.Random (runRand)
import System.Random (StdGen)
import Data.List (genericTake)

taxicab :: V → V → GLdouble
taxicab (Vector3 x y z) (Vector3 x' y' z') = abs (x-x') + abs (y-y') + abs (z-z')

betterThan :: [V] → Life → Life → Bool
betterThan target a b
    | al > bl * 1.4 = True
    | bl > al * 1.4 = False
    | otherwise = cost a * 1.3 < cost b
    where
      lookahead = 400
      cost = average . map (uncurry taxicab) . genericTake lookahead . zip target . positions
      al, bl :: Float
      al = fromInteger $ lifeExpectancyUpto lookahead a
      bl = fromInteger $ lifeExpectancyUpto lookahead b

data Stalker c = Stalker { stalker :: Life, prng :: StdGen, stalked :: c }
  -- Nice example of a controller modifier that needs some extra state in addition to just more lives.

instance BasicController c ⇒ BasicController (Stalker c) where
  controllerObstacles = controllerObstacles . stalked
  controllerGpCfg = controllerGpCfg . stalked

stalk :: BasicController c ⇒ Stalker c → c → Stalker c
stalk Stalker{..} stalked' = Stalker stalker' prng' stalked'
  where
    (stalker', prng') = case player stalked' of
      Just l → runRand (reviseIfWise (tryRandomAction (betterThan (positions l)) (controllerObstacles stalked) (controllerGpCfg stalked)) stalker) prng
      Nothing → (stalker, prng)

instance BasicController c ⇒ Controller (Stalker c) where
  player = player . stalked
  others Stalker{..} = stalker : others stalked
  tick c = stalk (c{stalker = future (stalker c)}) (tick (stalked c))
  fire g v c = stalk c `fmap` fire g v (stalked c)
