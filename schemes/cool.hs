module Configuration (config) where

import SlingSpace.Configuration

config :: Scheme
config = Scheme
  { shadow_color = Color4 0.68 0.7 0.72 1
  , grid_color = Color4 0.5 0.55 0.6 1
  , rope_line_width = 3
  , fog_density = 0.00008
  , fog_color = Color4 1 1 1 1
  , gunColor = const $ Color4 0.3 0.35 0.4 1
  , lightModel_ambient = Color4 0.4 0.4 0.4 1
  , ballLight_ambient = Color4 0.2 0.2 0.2 1
  , ballLight_diffuse = Color4 0.5 0.5 0.5 1
  , ball_material_ambient = Color4 1 0 0 1
  , ball_material_diffuse = Color4 1 1 1 1
  , ballLight_attenuation = (1.0, 0.0002, 0)
  }
