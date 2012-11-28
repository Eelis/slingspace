module Configuration (config) where

import SlingSpace.Configuration
import Data.Map (fromList)

g = GunConfig
  { ropeStrength = \x -> 0.013 * x ** 0.49
  , shootingSpeed = 75.0
  , shootingRange = 5000.0 }

config = GameplayConfig
  { gunConfigs = fromList [(LeftGun, g), (RightGun, g)]
  , friction = 0.995
  , gravity = Vector3 0 (-0.13) 0 }
