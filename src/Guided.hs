{-# LANGUAGE UnicodeSyntax, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Guided (Guided(..)) where

import Controllers (Controller(..), BasicController(..))
import Util (whenJust)
import Logic (Life, lifeExpectancyUpto)
import Control.Monad (liftM2, guard)
import Data.Function (on)

newtype Guided c = Guided { trainee :: c }

deriving instance BasicController c ⇒ BasicController (Guided c)

instance BasicController c ⇒ Controller (Guided c) where
  player = player . trainee
  others = others . trainee
  tick c = c{ trainee = tick (trainee c) }
  fire g v c = do
    t ← fire g v (trainee c)
    whenJust (liftM2 betterThan (player t) (player (trainee c))) guard
    return c{trainee=t}
   where
    betterThan :: Life → Life → Bool
    betterThan = (>=) `on` lifeExpectancyUpto 300
