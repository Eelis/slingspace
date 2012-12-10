{-# LANGUAGE UnicodeSyntax #-}

module Playback (playback) where

import Logic (Life, RefreshRate)
import Gui (gui, GuiConfig)
import Math (VisualObstacle, asStoredVertices)
import Obstacles (ObstacleTree)
import Graphics.Rendering.OpenGL.GL (GLdouble)
import Control.Monad (void)
import qualified SlingSpace.Configuration

playback :: [VisualObstacle] → ObstacleTree → GuiConfig → GLdouble → (RefreshRate → Life) → IO ()
  -- non-interactive display of a life
playback obstacles tree guiConfig viewDir life = void $ gui
  (asStoredVertices obstacles)
  tree
  guiConfig
  (const SlingSpace.Configuration.def)
  viewDir
  life
