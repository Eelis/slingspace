module SlingSpace.Configuration
  ( GameplayConfig(..)
  , Gun(..), GunConfig(..), GunGuiConfig(..), GuiConfig(..), Scheme(..)
  , Vector3(..), (<*>), (<+>), Color4(..)
  , defaultGunConfig, defaultGameplayConfig, defaultGuiConfig, defaultObstacleColor
  ) where

import Graphics.Rendering.OpenGL.GL
import Logic
import Gui
import Math
import Graphics.UI.GLUT (MouseButton(..), KeyState(..), Key(..))

defaultGunConfig :: GunConfig
defaultGunConfig = GunConfig
  { ropeStrength = \x -> 0.013 * x ** 0.49
  , shootingSpeed = 75.0
  , shootingRange = 5000.0 }

defaultGameplayConfig :: GameplayConfig
defaultGameplayConfig = GameplayConfig
  { gunConfig = const defaultGunConfig
  , applyForce =
      (<*> 0.995) . -- friction
      (<+> Vector3 0 (-0.13) 0) -- gravity
  }

defaultGuiConfig :: GuiConfig
defaultGuiConfig = GuiConfig
  { windowTitle = "SlingSpace"
  , gunGuiConfig = \g -> GunGuiConfig (-0.12) (case g of LeftGun -> -0.19; RightGun -> 0.19)
  , ugly = False
  , floorConf = Just (Grid { grid_size = 200, grid_type = LinedGrid {grid_line_width=3} })
  , playerSize = 35
  , camConf = CameraConfig
    { viewing_dist = 30000
    , fov = 45
    , cam_init_dist = 600
    , cam_min_dist = 40
    , cam_max_dist = 5000
    , cam_zoom_speed = 1.1
    , mouse_speed = 573
    , invert_mouse = False
    }
  , schemeFile = "cool.hs"
  , restart_key = (Char ' ', Down)
  , pause_key = (Char 'p', Down)
  , exit_key = (Char 'q', Down)
  , zoom_in_key = (MouseButton WheelUp, Down)
  , zoom_out_key = (MouseButton WheelDown, Down)
  }

defaultObstacleColor :: Color3 GLdouble
defaultObstacleColor = Color3 0.9 0.9 0.9
