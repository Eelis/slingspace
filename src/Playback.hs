
module Playback (playback) where

import Logic (Life(..))
import Gui (gui, Controller(..), GuiConfig)
import qualified Data.Map as Map
import Control.Monad.Fix (fix)
import Math (VisualObstacle, asStoredVertices)
import Obstacles (ObstacleTree)
import Graphics.Rendering.OpenGL.GL (GLdouble)
import qualified SlingSpace.Configuration

playbackController :: Life -> Controller
playbackController l = fix $ \self -> Controller
  { players = Map.singleton "jimmy" l
  , tick = case l of Life _ l' -> playbackController l'; Death _ -> self
  , release = const Nothing
  , Gui.fire = const (const Nothing) }

playback :: [VisualObstacle] -> ObstacleTree -> GuiConfig -> GLdouble -> Life -> IO ()
  -- non-interactive display of a life
playback obstacles tree guiConfig viewDir life = gui
  (asStoredVertices obstacles)
  tree
  "jimmy"
  guiConfig
  (const SlingSpace.Configuration.def)
  viewDir
  (playbackController life) >> return ()
