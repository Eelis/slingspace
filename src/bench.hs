{-# LANGUAGE RecordWildCards, UnicodeSyntax, ScopedTypeVariables #-}

import Logic (Player(..), tickPlayer)
import qualified Data.Map as Map
import Math (GeometricObstacle(..), Ray(..), Cube(..))
import MyGL ()
import MyUtil (read_config_file, getMonotonicMilliSecs)
import Graphics.UI.GLUT (Vector3(..))
import Obstacles (benchmark_tunnel)
import Control.Monad.Random (evalRandIO, evalRand)
import System.Random (mkStdGen)
import qualified Octree
import Control.DeepSeq (deepseq)

mega = 2^20
twomega = 2^21

main :: IO ()
main = do
  gp_cfg ← read_config_file "gameplay.txt"
  let
    tick :: Player → Player
    tick p = case tickPlayer tree gp_cfg p of
      Left collisionPos -> p{body=Ray collisionPos (Vector3 0 0 0)}
      Right p' -> p'
    tree = Octree.fromList (Cube (Vector3 (-mega) (-mega) (-mega)) twomega) obstacles
    obstacles = evalRand benchmark_tunnel (mkStdGen 4)
    initialPositions = [Vector3 x 50000 0 | x <- [-10..10]]
    initialPlayers = [Player (Ray p (Vector3 0 0 0)) Map.empty | p <- initialPositions]

  --putStrLn $ unlines $ map (show . (\(Vector3 _ y _) -> y) . rayOrigin . body) $ take 6000 $ iterate (tickPlayer tree gp_cfg) initialPlayer

  deepseq tree $ do

  t ← getMonotonicMilliSecs
  print $ [ body (iterate tick p !! 6000) | p <- initialPlayers]
  t' ← getMonotonicMilliSecs
  print $ t' - t
