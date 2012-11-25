{-# LANGUAGE RecordWildCards, UnicodeSyntax, ScopedTypeVariables #-}

import Gui (playback)
import Logic (Player(..), release, fire, Life(..), lifeAfter, lifeExpectancyUpto, live, GameplayConfig, Gun(LeftGun), moments)
import qualified Data.Map as Map
import Math (GeometricObstacle(..), Ray(..), Cube(..), triangleCenter, (<->))
import MyGL ()
import MyUtil ((.), read_config_file, randomItem)
import Data.List (genericLength)
import Graphics.UI.GLUT (Vector3(..))
import Obstacles (infinite_tunnel, bigCube, ObstacleTree)
import Prelude hiding ((.))
import Control.Monad.Random (evalRand, MonadRandom, getRandomR)
import System.Random (mkStdGen)
import qualified Octree

average :: Fractional b => [b] -> b
average l = sum l / fromInteger (genericLength l)

betterThan :: Life -> Life -> Bool
a `betterThan` b
    | abs (al - bl) > 200 = al > bl
    | otherwise = value a > value b
    where
      al = lifeExpectancyUpto lookahead a
      bl = lifeExpectancyUpto lookahead b
      value = average . map ((\(Vector3 _ y z) -> y+z) . rayOrigin . body) . take lookahead . moments
      lookahead = 400

considerAlternatives :: forall m . (Functor m, MonadRandom m) =>
  ObstacleTree → GameplayConfig → Life → m (Maybe (Player, Life))
considerAlternatives _ _ (Death _) = return Nothing -- too late
considerAlternatives obstacles gpCfg (Life now future) = do
  alternativeNow <- do
    i :: Int <- getRandomR (0, 10)
    if i == 0
      then return $ release LeftGun now
      else do
        c <- randomItem nearby >>= randomItem . map triangleCenter . obstacleTriangles
        return $ fire gpCfg LeftGun c now
  let alternativeFuture = lifeAfter obstacles gpCfg alternativeNow
  return $ if alternativeFuture `betterThan` future
    then Just (alternativeNow, alternativeFuture)
    else Nothing
 where
  s = 3000
  here = Cube (rayOrigin (body now) <-> Vector3 s s s) (s*2)
  nearby = Octree.query here obstacles

tarzan :: forall m . (Functor m, MonadRandom m) => ObstacleTree → GameplayConfig → Life → m Life
tarzan obstacles gpCfg = go
  where
    go :: Life → m Life
    go l@(Death _) = return l
    go l@(Life now future) = do
      ml' <- considerAlternatives obstacles gpCfg l
      case ml' of
        Nothing -> Life now `fmap` go future
        Just (x, y) -> Life x `fmap` go y

main :: IO ()
main = do
  tuCfg ← read_config_file "infinite-tunnel.txt"
  gpCfg ← read_config_file "gameplay.txt"

  let
    obstacles :: [GeometricObstacle] = take 1000 $ ((\(_, _, x) -> x) .) $ evalRand (infinite_tunnel tuCfg) (mkStdGen 4)
    tree = Octree.fromList bigCube obstacles
    initialPosition = Vector3 0 1800 (-2000)
    initialPlayer = Player (Ray initialPosition (Vector3 0 0 0)) Map.empty

  playback obstacles tree $ evalRand (tarzan tree gpCfg $ live tree gpCfg initialPlayer) (mkStdGen 3)
