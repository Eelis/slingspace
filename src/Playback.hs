
module Playback (playback) where

import Logic (Life(..))
import Gui (gui, Controller(..), GuiConfig)
import qualified Octree
import qualified Data.Map as Map
import Control.Monad.Fix (fix)
import Math (GeometricObstacle, VisualObstacle(..), asStoredVertices)
import SlingSpace.Configuration (defaultGunConfig, defaultObstacleColor)

playbackController :: Life -> Controller
playbackController l = fix $ \self -> Controller
  { players = Map.singleton "jimmy" l
  , tick = do
    return (Nothing, case l of Life _ l' -> playbackController l'; Death _ -> self)
  , release = const Nothing
  , Gui.fire = const (const Nothing) }

playback :: [GeometricObstacle] -> Octree.CubeBox GeometricObstacle -> GuiConfig -> Life -> IO ()
  -- non-interactive display of a life
playback obstacles tree guiConfig life = gui
  (playbackController life)
  ( asStoredVertices (map (VisualObstacle defaultObstacleColor) obstacles)
  , tree)
  "jimmy"
  guiConfig
  (const defaultGunConfig)
