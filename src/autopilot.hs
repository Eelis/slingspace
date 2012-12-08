{-# LANGUAGE RecordWildCards, UnicodeSyntax, ScopedTypeVariables #-}

import Playback (playback)
import Logic (Player(..), Life(..), lifeExpectancyUpto, live, moments, keepTrying, tryRandomAction)
import qualified Data.Map as Map
import Math (GeometricObstacle(..), Ray(..), VisualObstacle(..))
import Util ((.), average, loadConfig, getDataFileName, getMonotonicMilliSecs)
import Data.List (genericTake)
import Graphics.Rendering.OpenGL.GL (Vector3(..))
import Obstacles (grow, randomObs)
import Prelude hiding ((.))
import Control.Monad (replicateM)
import Control.Monad.Random (evalRand, MonadRandom(..))
import System.Random (mkStdGen)
import Control.DeepSeq (deepseq)
import qualified SlingSpace.Configuration

betterThan :: Life → Life → Bool
a `betterThan` b
    | abs (al - bl) > 200 = al > bl
    | otherwise = value a > value b
    where
      al = lifeExpectancyUpto lookahead a
      bl = lifeExpectancyUpto lookahead b
      value = average . map ((\(Vector3 _ y z) → y * 3 + z) . rayOrigin . body) . genericTake lookahead . moments
      lookahead = 400

rawObstacles :: (Functor m, MonadRandom m) ⇒ m [GeometricObstacle]
rawObstacles = replicateM 300 $
  getRandomR (Vector3 (-1200) 0 0, Vector3 1200 2000 100000)
    >>= randomObs 600

benchmark :: Bool
benchmark = False

main :: IO ()
main = do
  gpCfg ← getDataFileName "config/gameplay.hs" >>= loadConfig
  guiConfig ← getDataFileName "config/gui.hs" >>= loadConfig

  let
    (tree, obstacles) = grow $ evalRand rawObstacles (mkStdGen 4)
    initialPosition = Vector3 0 1800 (-2000)
    initialPlayer = Player (Ray initialPosition (Vector3 0 0 0)) Map.empty
    life = evalRand (keepTrying (tryRandomAction betterThan tree gpCfg) (live tree gpCfg initialPlayer)) (mkStdGen 3)

  if benchmark
    then deepseq tree $ do
      t ← getMonotonicMilliSecs
      print $ length $ moments life
      t' ← getMonotonicMilliSecs
      print $ t' - t
    else playback
      (map (VisualObstacle SlingSpace.Configuration.defaultObstacleColor) obstacles)
      tree guiConfig pi
      life
