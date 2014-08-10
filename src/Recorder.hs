{-# LANGUAGE UnicodeSyntax, RecordWildCards #-}

module Recorder (Recorder(..), record) where

import Logic (Player, birth)
import Controllers (Controller(..), players)
import Data.Maybe (mapMaybe)

data Recorder c = Recorder { frames :: [[Player]], recordee :: c }

instance Controller c ⇒ Controller (Recorder c) where
  player = player . recordee
  others = others . recordee
  tick Recorder{..} = Recorder (mapMaybe birth (players recordee) : frames) (tick recordee)
  fire g v Recorder{..} = Recorder frames `fmap` fire g v recordee
  onChar Recorder{..} ch = Recorder frames `fmap` onChar recordee ch

record :: c → Recorder c
record = Recorder []
