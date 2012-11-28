module SlingSpace.Configuration
  ( GameplayConfig(..)
  , Gun(..), GunConfig(..), GunGuiConfig(..), GuiConfig(..), Scheme(..), CameraConfig(..), CameraOrientation(..)
  , Vector3(..), (<*>), (<+>), Color4(..)
  , def, defaultObstacleColor
  ) where

import Graphics.Rendering.OpenGL.GL
import Logic
import Gui
import Math
import Graphics.UI.GLUT (MouseButton(..), KeyState(..), Key(..))

class Default a where def :: a

instance Default GunConfig where
  def = GunConfig
    { ropeStrength = (0.013 *) . (** 0.49)
    , shootingSpeed = 75.0
    , shootingRange = 5000.0 }

instance Default GameplayConfig where
  def = GameplayConfig
    { gunConfig = const def
    , applyForce =
        (<*> 0.995) . -- friction
        (<+> Vector3 0 (-0.13) 0) -- gravity
    }

instance Default CameraConfig where
  def = CameraConfig
    { viewing_dist = 30000
    , fov = 45
    , zoomIn = max 40 . (/ 1.1)
    , zoomOut = min 5000 . (* 1.1)
    , mouse_speed = 573
    , invert_mouse = False }

instance Default GuiConfig where
  def = GuiConfig
    { windowTitle = "SlingSpace"
    , gunGuiConfig = GunGuiConfig (-0.12) . (\g -> case g of LeftGun -> -0.19; RightGun -> 0.19)
    , ugly = False
    , floorConf = Just (Grid { grid_size = 200, grid_type = LinedGrid {grid_line_width=3} })
    , playerSize = 35
    , camConf = def
    , schemeFile = "cool.hs"
    , restart_key = (Char ' ', Down)
    , pause_key = (Char 'p', Down)
    , exit_key = (Char 'q', Down)
    , zoom_in_key = (MouseButton WheelUp, Down)
    , zoom_out_key = (MouseButton WheelDown, Down)
    , tickDuration = 10 }

instance Default CameraOrientation where
  def = CameraOrientation 600 0 pi

defaultObstacleColor :: Color3 GLdouble
defaultObstacleColor = Color3 0.9 0.9 0.9


