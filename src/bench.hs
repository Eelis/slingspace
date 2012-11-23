{-# LANGUAGE RecordWildCards, UnicodeSyntax, ScopedTypeVariables #-}

import Logic (Player(..), tickPlayer)
import qualified Data.Map as Map
import Math (GeometricObstacle(..), Ray(..), Cube(..))
import MyGL ()
import MyUtil (read_config_file)
import Graphics.UI.GLUT (Vector3(..))
import Obstacles (benchmark_tunnel)
import Control.Monad.Random (evalRandIO, evalRand)
import System.Random (mkStdGen)
import qualified Octree

mega = 2^20
twomega = 2^21

main :: IO ()
main = do
  gp_cfg ← read_config_file "gameplay.txt"
  let
    tree = Octree.fromList (Cube (Vector3 (-mega) (-mega) (-mega)) twomega) obstacles
    obstacles = evalRand benchmark_tunnel (mkStdGen 4)
    initialPosition = Vector3 0 50000 0
    initialPlayer = Player (Ray initialPosition (Vector3 0 0 0)) Map.empty False

  --putStrLn $ unlines $ map (show . (\(Vector3 _ y _) -> y) . rayOrigin . body) $ take 6000 $ iterate (tickPlayer tree gp_cfg) initialPlayer

  print $ body $ iterate (tickPlayer tree gp_cfg) initialPlayer !! 6000
