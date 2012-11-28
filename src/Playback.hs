
module Playback (playback) where

import Logic (Life(..))
import Gui (gui, Controller(..), GuiConfig)
import qualified Data.Map as Map
import Control.Monad.Fix (fix)
import Math (VisualObstacle, asStoredVertices)
import Obstacles (ObstacleTree)
import SlingSpace.Configuration (defaultGunConfig)

playbackController :: Life -> Controller
playbackController l = fix $ \self -> Controller
  { players = Map.singleton "jimmy" l
  , tick = do
    return (Nothing, case l of Life _ l' -> playbackController l'; Death _ -> self)
  , release = const Nothing
  , Gui.fire = const (const Nothing) }

playback :: [VisualObstacle] -> ObstacleTree -> GuiConfig -> Life -> IO ()
  -- non-interactive display of a life
playback obstacles tree guiConfig life = gui
  (playbackController life)
  (asStoredVertices obstacles, tree)
  "jimmy"
  guiConfig
  (const defaultGunConfig)
