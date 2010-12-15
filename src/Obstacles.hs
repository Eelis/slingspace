module Obstacles
  ( niceTunnel, bigField
  , infinite_tunnel
  , TunnelConfig
  ) where

import MyUtil
import Math
import MyGL ()
import System.Random
import Control.Monad
import Graphics.UI.GLUT
import Prelude hiding ((.))
import Control.Arrow (first)


{-
aboveSurface :: (Ord a, Floating a) ⇒ Vector3 a → Bool
aboveSurface (Vector3 _ y _) = y > 0

aboveSurfaceTri :: AnnotatedTriangle → Bool
aboveSurfaceTri (AnnotatedTriangle _ (x, y, z) _) =
  aboveSurface x && aboveSurface y && aboveSurface z

aboveSurfaceObs :: AnnotatedObstacle → Bool
aboveSurfaceObs (AnnotatedObstacle a b c d) = and $ map aboveSurfaceTri [a, b, c, d]
-}

randomObs :: RandomGen g ⇒ V → GLdouble → RngMonad g AnnotatedObstacle
randomObs center size = do
  let q = (center <+>) . randomVector3 size
  x ← q; y ← q; z ← q
  let w = x <-> (normalize_v (cross_prod (y <-> x) (z <-> x)) <*> size)
  return $ annotate_obstacle $ [at x y z, at y x w, at x z w, at z y w]
  where at = annotate_triangle

data TunnelConfig = TunnelConfig
  { allow_intersecting_obstacles :: Bool -- can improve performance, but causes artifacts
  , obstacle_density :: GLdouble -- average distance between successive obstacles
  , obstacle_size, init_tunnel_width, max_tunnel_width, min_tunnel_width :: GLdouble
  } deriving (Show, Read)

tunnel :: RandomGen g ⇒ TunnelConfig → [AnnotatedObstacle] → V → GLdouble → GLdouble → V → RngMonad g [AnnotatedObstacle]
tunnel cf prev dir@(Vector3 dx dy dz) width obssize from = do
  coff ← randomRM (Vector3 (-width) (-width) 0, Vector3 width width 0)
  newobst ← randomObs (from <+> coff) obssize
  if (not (allow_intersecting_obstacles cf) && any (obst_obst_collision newobst) prev) || obst_min_y newobst < 0
   then tunnel cf prev dir width obssize from
   else do
    xChange ← randomRM (-20, 20)
    yChange ← randomRM (-22, 20)
    newwidth ← bounded (min_tunnel_width cf) (max_tunnel_width cf) . (width +) . randomRM (-200, 200)
    let Vector3 nfx nfy nfz = from <+> dir
    let od = obstacle_density cf
    (newobst :) . tunnel cf (newobst : take 10 prev)
      (Vector3
        (bounded (dx + xChange) (-od) od)
        (bounded (dy + yChange) (-od) od)
        dz) newwidth obssize (Vector3 nfx (bounded nfy width 6500) nfz)

niceTunnel :: RandomGen g ⇒ TunnelConfig → RngMonad g [AnnotatedObstacle]
niceTunnel cf = take 150 . tunnel cf []
  (Vector3 0 0 (- obstacle_density cf)) -- dir
  (init_tunnel_width cf)
  (obstacle_size cf) -- obstacle size
  (Vector3 0 1000 0) -- from

bigField :: RandomGen g ⇒ RngMonad g [AnnotatedObstacle]
bigField = replicateM 1600 $ do
  c ← randomRM (Vector3 0 500 0, Vector3 56000 2000 56000)
  randomObs c 800

{-
data GraphNode = GraphNode V [GraphNode]

to_graphnode :: [V] → V → GraphNode
to_graphnode l v =
  GraphNode v $ map (to_graphnode l) $ filter (\w → dist_sqrd v w < 1000) l
-}
{-
graph_to_graphnode :: Graph → Vertex → GraphNode
graph_to_graphnode g v = GraphNode v $ map (graph_to_graphnode g) $ g !! v
-}

{-
data InfiniteTunnelConfig = InfiniteTunnelConfig
  { allow_intersecting_obstacles :: Bool -- can improve performance, but causes artifacts
  , obstacle_density :: GLdouble -- in average distance between successive obstacles
  , obstacle_size, init_tunnel_width, max_tunnel_width, min_tunnel_width :: GLdouble
  } deriving (Show, Read)
-}

glDouble :: Double → GLdouble
glDouble = realToFrac
unGLdouble :: GLdouble → Double
unGLdouble = realToFrac
  -- Todo: These are horrible, and were added just to support the following instance, needed to make things compile again now that GLdouble is a newtype with a hidden constructor.

instance Random GLdouble where
  randomR (lo, hi) = first glDouble . randomR (unGLdouble lo, unGLdouble hi)
  random = first glDouble . random

infinite_tunnel :: RandomGen g ⇒ TunnelConfig → RngMonad g [(V, V, AnnotatedObstacle)]
infinite_tunnel cf = tu [] 0 {-(pi * 0.5)-} {- ang -} (init_tunnel_width cf) (Vector3 0 0 0) {- from -}
  where
    tu :: RandomGen g ⇒ [AnnotatedObstacle] → GLdouble → GLdouble → V → RngMonad g [(V, V, AnnotatedObstacle)]
    tu prev ang width from = do
      coff ← randomRM (Vector3 (-width) 0 0, Vector3 width (2 * width) 0)
      newobst ← randomObs (from <+> coff) (obstacle_size cf)
      if (not (allow_intersecting_obstacles cf) && any (obst_obst_collision newobst) prev) {-|| obst_min_y newobst < 0-}
       then {-trace "<miss>" $-} tu prev ang width from
       else do
        let d = Vector3 (sin ang) 0 (cos ang)
        widthChange ← randomRM (-200, 200)
        let angChange = 0
        --angChange ← randomRM (-0.2, 0.2)
        {-trace "<generating>" $-}
        ((from, d, newobst) :) . tu (newobst : take 10 prev) (ang + angChange)
          (bounded (width + widthChange) (min_tunnel_width cf) (max_tunnel_width cf))
          (from <+> (d <*> obstacle_density cf))
