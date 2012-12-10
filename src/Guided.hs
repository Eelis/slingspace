{-# LANGUAGE UnicodeSyntax, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Guided (Guided(..)) where

import Controllers (Controller(..), BasicController(..))
import Logic (Life, lifeExpectancyUpto)
import Control.Monad (guard)
import Data.Function (on)

newtype Guided c = Guided { trainee :: c }

deriving instance BasicController c ⇒ BasicController (Guided c)

instance BasicController c ⇒ Controller (Guided c) where
  player = player . trainee
  others = others . trainee
  tick c = c{ trainee = tick (trainee c) }
  fire g v c = do
    t ← fire g v (trainee c)
    guard (player t `betterThan` player (trainee c))
    return c{trainee=t}
   where
    betterThan :: Life → Life → Bool
    betterThan = (>=) `on` lifeExpectancyUpto 300
