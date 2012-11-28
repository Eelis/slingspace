module Configuration (config) where

import SlingSpace.Configuration
import Data.Map (fromList)

config = GameplayConfig
  { gunConfigs = fromList
    [ ( LeftGun
      , GunConfig
        { ropeForceScale = 0.013
        , ropeForceExponent = 0.49
        , shootingSpeed = 75.0
        , shootingRange = 5000.0 } )
    , ( RightGun
      , GunConfig
        { ropeForceScale = 0.013
        , ropeForceExponent = 0.49
        , shootingSpeed = 75.0
        , shootingRange = 5000.0 } )
    ]
  , friction = 0.995
  , gravity = Vector3 0 (-0.13) 0 }
