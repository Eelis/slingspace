{-# LANGUAGE UnicodeSyntax, LambdaCase #-}

module SlingSpace.Configuration
  ( GameplayConfig(..)
  , Gun(..), GunConfig(..), GunGuiConfig(..), GuiConfig(..), Scheme(..), CameraConfig(..)
  , Vector3(..), Color4(..)
  , def, defaultObstacleColor
  ) where

import Graphics.Rendering.OpenGL.GL
import Logic (GunConfig(..), GameplayConfig(..), Gun(..))
import Gui (GuiConfig(..), GunGuiConfig(..), CameraConfig(..), FloorConfig(..), GridType(..), Scheme(..))
import qualified Graphics.UI.GLFW as GLFW

class Default a where def :: a

instance Default GunConfig where
  def = GunConfig
    { ropeStrength = (130 *) . (** 0.49)
    , shootingSpeed = 7500
    , shootingRange = 5000.0 }

instance Default GameplayConfig where
  def = GameplayConfig
    { gunConfig = const def
    , gravity = Vector3 0 (-1050) 0
    , drag = 0.7 }

instance Default CameraConfig where
  def = CameraConfig
    { viewing_dist = 50000
    , fov = 45
    , wheelBounds = (-10, 5)
    , zoom = fromIntegral . (120 +) . (* 20) . (^ (2::Int)) . (5 -)
    , mouse_speed = 573
    , invert_mouse = True }

instance Default GuiConfig where
  def = GuiConfig
    { windowTitle = "SlingSpace"
    , gunGuiConfig = GunGuiConfig (-0.12) . (\case LeftGun → -0.19; RightGun → 0.19)
    , ugly = False
    , floorConf = Just (Grid { grid_size = 200, grid_type = LinedGrid {grid_line_width=3} })
    , playerSize = 35
    , camConf = def
    , schemeFile = "cool.hs"
    , restart_key = GLFW.CharKey ' '
    , pause_key = GLFW.CharKey 'P'
    , gunForButton = \case
        GLFW.MouseButton0 → Just LeftGun
        GLFW.MouseButton1 → Just RightGun
        _ → Nothing }

defaultObstacleColor :: Color3 GLdouble
defaultObstacleColor = Color3 0.9 0.9 0.9
