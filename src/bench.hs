{-# LANGUAGE RecordWildCards, UnicodeSyntax, ScopedTypeVariables #-}

import Logic (Player(..), tickPlayer)
import qualified Data.Map as Map
import Math (Ray(..), GeometricObstacle, V, (<+>))
import MyUtil (getMonotonicMilliSecs, loadConfig, getDataFileName)
import Graphics.UI.GLUT (Vector3(..))
import Obstacles (bigCube, randomObs)
import Control.Monad.Random (MonadRandom, evalRand, getRandomR)
import System.Random (mkStdGen)
import qualified Octree
import Control.DeepSeq (deepseq)

tunnel :: forall m . (Functor m, MonadRandom m) ⇒ m [GeometricObstacle]
tunnel = take 1000 `fmap` tu (Vector3 0 0 0)
  where
    width = 1500
    obsSize = 800
    tu :: V → m [GeometricObstacle]
    tu from = do
      coff ← getRandomR (Vector3 (-width) 0 0, Vector3 width (2 * width) 0)
      newobst ← randomObs (from <+> coff) obsSize
      (newobst :) `fmap` tu (from <+> Vector3 0 300 0)

main :: IO ()
main = do
  gpCfg ← getDataFileName "config/gameplay.hs" >>= loadConfig
  let
    tick :: Player → Player
    tick p = case tickPlayer tree gpCfg p of
      Left collisionPos -> p{body=Ray collisionPos (Vector3 0 0 0)}
      Right p' -> p'
    tree = Octree.fromList bigCube obstacles
    obstacles = evalRand tunnel (mkStdGen 4)
    initialPositions = [Vector3 x 50000 0 | x <- [-10..10]]
    initialPlayers = [Player (Ray p (Vector3 0 0 0)) Map.empty | p <- initialPositions]

  --putStrLn $ unlines $ map (show . (\(Vector3 _ y _) -> y) . rayOrigin . body) $ take 6000 $ iterate (tickPlayer tree gp_cfg) initialPlayer

  deepseq tree $ do

  t ← getMonotonicMilliSecs
  print $ [ body (iterate tick p !! 6000) | p <- initialPlayers]
  t' ← getMonotonicMilliSecs
  print $ t' - t
