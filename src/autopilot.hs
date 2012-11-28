{-# LANGUAGE RecordWildCards, UnicodeSyntax, ScopedTypeVariables #-}

import Gui (playback)
import Logic (Player(..), Life(..), lifeExpectancyUpto, live, moments, keepTrying, tryRandomAction)
import qualified Data.Map as Map
import Math (GeometricObstacle(..), Ray(..))
import MyGL ()
import MyUtil ((.), read_config_file, average, loadConfig)
import Data.List (genericTake)
import Graphics.UI.GLUT (Vector3(..))
import Obstacles (infinite_tunnel, bigCube)
import Prelude hiding ((.))
import Control.Monad.Random (evalRand)
import System.Random (mkStdGen)
import qualified Octree

betterThan :: Life -> Life -> Bool
a `betterThan` b
    | abs (al - bl) > 200 = al > bl
    | otherwise = value a > value b
    where
      al = lifeExpectancyUpto lookahead a
      bl = lifeExpectancyUpto lookahead b
      value = average . map ((\(Vector3 _ y z) -> y+z) . rayOrigin . body) . genericTake lookahead . moments
      lookahead = 400

main :: IO ()
main = do
  tuCfg ← read_config_file "infinite-tunnel.txt"
  gpCfg ← loadConfig "gameplay"

  let
    obstacles :: [GeometricObstacle] = take 1000 $ ((\(_, _, x) -> x) .) $ evalRand (infinite_tunnel tuCfg) (mkStdGen 4)
    tree = Octree.fromList bigCube obstacles
    initialPosition = Vector3 0 1800 (-2000)
    initialPlayer = Player (Ray initialPosition (Vector3 0 0 0)) Map.empty
    life = keepTrying (tryRandomAction betterThan tree gpCfg) (live tree gpCfg initialPlayer)

  playback obstacles tree $ evalRand life (mkStdGen 3)
