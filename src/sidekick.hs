{-# LANGUAGE RecordWildCards, UnicodeSyntax, ScopedTypeVariables, ViewPatterns, NamedFieldPuns #-}

import Gui (gui)
import qualified Gui
import Logic (Player(..), release, fire, Life(..), lifeExpectancyUpto, live, GameplayConfig(..), future, immortalize, reviseIfWise, positions, tryRandomAction)
import qualified Data.Map as Map
import Math (VisualObstacle(..), GeometricObstacle(..), Ray(..), asStoredVertices, V)
import MyGL ()
import MyUtil ((.), read_config_file, average, loadConfig)
import Graphics.UI.GLUT (Vector3(..), GLdouble)
import Obstacles (infinite_tunnel, bigCube)
import Prelude hiding ((.))
import Data.List (genericTake)
import Control.Monad.Random (evalRandIO, runRand)
import System.Random (mkStdGen, StdGen)
import qualified Octree
import qualified SlingSpace.Configuration

playerName :: String
playerName = "Player"

taxicab :: V -> V -> GLdouble
taxicab (Vector3 x y z) (Vector3 x' y' z') = abs (x-x') + abs (y-y') + abs (z-z')

betterThan :: [V] -> Life -> Life -> Bool
betterThan target a b
    | al > bl * 1.4 = True
    | bl > al * 1.4 = False
    | otherwise = cost a * 1.3 < cost b
    where
      lookahead = 400
      cost = average . map (uncurry taxicab) . genericTake lookahead . zip target . positions
      al, bl :: Float
      al = fromInteger $ lifeExpectancyUpto lookahead a
      bl = fromInteger $ lifeExpectancyUpto lookahead b

main :: IO ()
main = do
  tuCfg ← read_config_file "infinite-tunnel.txt"
  gpCfg@GameplayConfig{gunConfig} ← loadConfig "config/gameplay.hs"
  guiConfig ← loadConfig "config/gui.hs"

  obstacles :: [GeometricObstacle] ← take 1000 . ((\(_, _, x) -> x) .) . evalRandIO (infinite_tunnel tuCfg)

  let
    tree = Octree.fromList bigCube obstacles
    liveForever = immortalize tree gpCfg . live tree gpCfg
    vertices = asStoredVertices (map (VisualObstacle SlingSpace.Configuration.defaultObstacleColor) obstacles)

    makeController :: Life -> Life -> StdGen -> Gui.Controller
    makeController (Death _) _ _ = error "died"
    makeController l@(Life p _) ai prng = Gui.Controller
      { players = Map.fromList [(playerName, l), ("HAL", ai)]
      , tick = return (Nothing, mc (future l) (future ai) prng)
      , release = \g -> Just $ mc (liveForever $ release g p) ai prng
      , fire = \g v -> Just $ mc (liveForever $ fire (gunConfig g) g v p) ai prng }
    mc altered oldAi prng = makeController altered alteredAI prng'
      where (alteredAI, prng') = runRand (reviseIfWise (tryRandomAction (betterThan (positions altered)) tree gpCfg) oldAi) prng

  gui
    (makeController
      (liveForever -- player
        (Player (Ray (Vector3 0 4000 (-3000)) (Vector3 0 0 0)) Map.empty))
      (live tree gpCfg -- ai (not immortal otherwise it won't be motivated to try to survive)
        (Player (Ray (Vector3 0 1800 (-2000)) (Vector3 0 0 0)) Map.empty))
      (mkStdGen 3))
    (vertices, tree)
    playerName
    guiConfig
    gunConfig
    SlingSpace.Configuration.def
    
