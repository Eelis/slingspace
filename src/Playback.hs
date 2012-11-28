
module Playback (playback) where

import Logic (Life(..))
import Gui (gui, Controller(..), GuiConfig, CameraOrientation)
import qualified Data.Map as Map
import Control.Monad.Fix (fix)
import Math (VisualObstacle, asStoredVertices)
import Obstacles (ObstacleTree)
import qualified SlingSpace.Configuration

playbackController :: Life -> Controller
playbackController l = fix $ \self -> Controller
  { players = Map.singleton "jimmy" l
  , tick = do
    return (Nothing, case l of Life _ l' -> playbackController l'; Death _ -> self)
  , release = const Nothing
  , Gui.fire = const (const Nothing) }

playback :: [VisualObstacle] -> ObstacleTree -> GuiConfig -> CameraOrientation -> Life -> IO ()
  -- non-interactive display of a life
playback obstacles tree guiConfig cam life = gui
  (playbackController life)
  (asStoredVertices obstacles, tree)
  "jimmy"
  guiConfig
  (const SlingSpace.Configuration.def)
  cam
