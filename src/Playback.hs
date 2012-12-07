{-# LANGUAGE UnicodeSyntax #-}

module Playback (playback) where

import Logic (Life)
import Gui (gui, GuiConfig)
import Math (VisualObstacle, asStoredVertices)
import Obstacles (ObstacleTree)
import Graphics.Rendering.OpenGL.GL (GLdouble)
import qualified SlingSpace.Configuration

playback :: [VisualObstacle] → ObstacleTree → GuiConfig → GLdouble → Life → IO ()
  -- non-interactive display of a life
playback obstacles tree guiConfig viewDir life = gui
  (asStoredVertices obstacles)
  tree
  guiConfig
  (const SlingSpace.Configuration.def)
  viewDir
  life >> return ()
