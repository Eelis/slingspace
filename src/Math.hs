{-# LANGUAGE RecordWildCards, ViewPatterns, BangPatterns, UnicodeSyntax, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Math where

import Prelude hiding ((.))
import MyGL ()
import Graphics.Rendering.OpenGL.GL hiding (Plane)
import Control.Monad
import MyUtil ((.), tupleToList)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (isJust, mapMaybe)
import Control.Monad.Random (Random(..), MonadRandom(..), runRand)

-- RANDOM STUFF

instance Random a ⇒ Random (Vector3 a) where
  random = runRand $ liftM3 Vector3 getRandom getRandom getRandom
  randomR (Vector3 lx ly lz, Vector3 hx hy hz) = runRand $
    liftM3 Vector3 (getRandomR (lx, hx)) (getRandomR (ly, hy)) (getRandomR (lz, hz))

instance Random a ⇒ Random (Color4 a) where
  random = runRand $ liftM4 Color4 getRandom getRandom getRandom getRandom
  randomR (Color4 lr lg lb la, Color4 hr hg hb ha) = runRand $
    liftM4 Color4 (getRandomR (lr, hr)) (getRandomR (lg, hg)) (getRandomR (lb, hb)) (getRandomR (la, ha))

randomVector3 :: (Random a, Num a, MonadRandom m) ⇒ a → m (Vector3 a)
randomVector3 size = getRandomR (Vector3 (-size) (-size) (-size), Vector3 size size size)

-- GEOMETRY

type V = Vector3 GLdouble

(<+>) :: Num a ⇒ Vector3 a → Vector3 a → Vector3 a
Vector3 x y z <+> Vector3 x' y' z' = Vector3 (x + x') (y + y') (z + z')

(<->) :: Num a ⇒ Vector3 a → Vector3 a → Vector3 a
Vector3 x y z <-> Vector3 x' y' z' = Vector3 (x - x') (y - y') (z - z')

(<*>) :: Num a ⇒ Vector3 a → a → Vector3 a
Vector3 x y z <*> s = Vector3 (x * s) (y * s) (z * s)

(</>) :: Fractional a ⇒ Vector3 a → a → Vector3 a
Vector3 x y z </> s = Vector3 (x / s) (y / s) (z / s)

tov :: Vector3 a → Vertex3 a
tov (Vector3 !x !y !z) = Vertex3 x y z

vectorToNormal :: Vector3 a → Normal3 a
vectorToNormal (Vector3 !x !y !z) = Normal3 x y z

norm_2 :: Floating a ⇒ Vector3 a → a
norm_2 (Vector3 !x !y !z) = sqrt $ x*x + y*y + z*z

normalize_v :: Floating a ⇒ Vector3 a → Vector3 a
normalize_v v = v </> (norm_2 v)

cross_prod :: Num a ⇒ Vector3 a → Vector3 a → Vector3 a
cross_prod (Vector3 x y z) (Vector3 x' y' z') =
  Vector3 ((y * z') - (z * y')) ((z * x') - (x * z')) ((x * y') - (y * x'))

inner_prod :: Num a ⇒ Vector3 a → Vector3 a → a
inner_prod (Vector3 x y z) (Vector3 x' y' z') = x*x' + y*y' + z*z'

{-# INLINE inner_prod #-}

data AnnotatedTriangle = AnnotatedTriangle
  { triangleNormal :: !V
  , triangleVertices :: !(V, V, V)
    -- TODO: can't we make the V's inside the tuple strict? are tuples strict?
  , obs_projs :: !(V, V, V) -- allow for fast calculation of ray intersections
  , triangleCenter :: !V
  } -- should only ever be constructed using annotate_triangle
  deriving (Show, Read, Eq)

annotateTriangle :: V → V → V → AnnotatedTriangle
annotateTriangle a b c =
  AnnotatedTriangle
    (normalize_v (cross_prod (b <-> a) (c <-> a)))
    (a, b, c)
    (kat a b c, kat b c a, kat c a b)
    ((a <+> b <+> c) </> 3)
  where
   kat a' b' c' = c' <-> ((natob <*> inner_prod natob (c' <-> a')) <+> a')
    where natob = normalize_v (b' <-> a')

data Ray = Ray { rayOrigin, rayDirection :: !V } deriving (Read, Show)
data Plane = Plane { planeNormal, planePoint :: !V }

plane :: AnnotatedTriangle → Plane
plane (AnnotatedTriangle n (a, _, _) _ _) = Plane n a

class Collision a b c | a b → c where collision :: a → b → c

data Sphere = Sphere { sphereCenter :: V, sphereSquaredRadius :: GLdouble }
  deriving (Read, Show)

instance Collision Ray Sphere Bool where
  collision Ray{..} Sphere{..} = b*b - 4*a*c >= 0
    where
      a = inner_prod rayDirection rayDirection
      b = 2 * inner_prod rayDirection o
      c = inner_prod o o - sphereSquaredRadius
      o = rayOrigin <-> sphereCenter

sameDirection :: V → V → Bool
sameDirection a b = inner_prod a b >= 0

instance Collision (Ray, GLdouble → V → Bool) AnnotatedTriangle (Maybe (GLdouble, V)) where
  collision (ray@Ray{..}, inter_pred) t@(AnnotatedTriangle _ (a, b, c) (anp, bp, cp) _) = do
    eta ← ray `collision` plane t
    let coll = rayOrigin <+> (rayDirection <*> eta)
    guard $ eta >= 0 && inter_pred eta coll &&
      sameDirection anp (coll <-> a) &&
      sameDirection bp (coll <-> b) &&
      sameDirection cp (coll <-> c)
    return (eta, coll)
      -- The predicate is integrated because applying it after-the-fact is more expensive, because by that time the three inner_prods have already been evaluated.

instance Collision (Ray, GLdouble → V → Bool) [AnnotatedTriangle] (Maybe (GLdouble, V, AnnotatedTriangle)) where
  collision ray triangles
      | null collisions = Nothing
      | otherwise = Just $ minimumBy (compare `on` (\(_,x,_) → x)) collisions
    where
      collisions :: [(GLdouble, V, AnnotatedTriangle)]
      collisions = (\(x,(y,z)) → (y,z,x)) . mapMaybe (\triangle → (,) triangle . (ray `collision` triangle)) triangles

instance Collision Ray Plane (Maybe GLdouble) where
  collision Ray{..} Plane{..}
    | u < 0 = Just $ inner_prod planeNormal (planePoint <-> rayOrigin) / u
    | otherwise = Nothing
   where u = inner_prod planeNormal rayDirection

rayThrough :: V → V → Ray
rayThrough from to = Ray from (to <-> from)

lineLoop :: [a] → [(a, a)]
lineLoop [] = []
lineLoop (x:xs) = go x xs
  where
    go l [] = [(l, x)]
    go l (y:ys) = (l, y) : go y ys

instance Collision AnnotatedTriangle AnnotatedTriangle Bool where
  collision t t' = or $
      h t' . lineLoop (tupleToList $ triangleVertices t) ++ h t . lineLoop (tupleToList $ triangleVertices t')
    where
      h :: AnnotatedTriangle → (V, V) → Bool
      h u (x, y) = isJust $  (rayThrough x y, \(z::GLdouble) (_::V) → z < 1) `collision` u

data GeometricObstacle = GeometricObstacle
  { obstacleSphere :: !Sphere
  , obstacleTriangles :: [AnnotatedTriangle]
  } deriving (Show, Read)

data VisualObstacle = VisualObstacle
  { geometricObstacle :: GeometricObstacle
  , obstacleColor :: Color4 GLfloat }

squaredDistance :: V → V → GLdouble
squaredDistance a b = inner_prod d d
  where d = a <-> b

annotateObstacle :: [AnnotatedTriangle] → GeometricObstacle
annotateObstacle obstacleTriangles = GeometricObstacle{obstacleSphere=Sphere{..}, ..}
  where
    sphereCenter = foldr1 (<+>) (triangleCenter . obstacleTriangles) </> realToFrac (length obstacleTriangles)
    sphereSquaredRadius = maximum $ squaredDistance sphereCenter . (obstacleTriangles >>= tupleToList . triangleVertices)

behind :: V → Plane → Bool
behind v Plane{..} = sameDirection (planePoint <-> v) planeNormal

point_in_obstacle :: GeometricObstacle → V → Bool
point_in_obstacle o v = and $ behind v . plane . obstacleTriangles o

tri_in_obstacle :: GeometricObstacle → AnnotatedTriangle → Bool
tri_in_obstacle o = or . (point_in_obstacle o .) .  tupleToList . triangleVertices

instance Collision GeometricObstacle GeometricObstacle Bool where
  collision a b = or [x `collision` y | x ← obstacleTriangles a, y ← obstacleTriangles b]

at_max_z :: AnnotatedTriangle → GLdouble
at_max_z (AnnotatedTriangle _ (Vector3 _ _ z0, Vector3 _ _ z1, Vector3 _ _ z2) _ _) =
  z0 `max` z1 `max` z2

at_min_y :: AnnotatedTriangle → GLdouble
at_min_y (AnnotatedTriangle _ (Vector3 _ y0 _, Vector3 _ y1 _, Vector3 _ y2 _) _ _) =
  y0 `min` y1 `min` y2

obst_min_y :: GeometricObstacle → GLdouble
obst_min_y = minimum . (at_min_y .) . obstacleTriangles

data Num a ⇒ MMatrix a = MMatrix a a a a a a a a a deriving Show

idMatrix :: Num a ⇒ MMatrix a
idMatrix = MMatrix 1 0 0 0 1 0 0 0 1

multMMatrix :: Num a ⇒ MMatrix a → MMatrix a → MMatrix a
multMMatrix (MMatrix a00 a01 a02 a10 a11 a12 a20 a21 a22) (MMatrix b00 b01 b02 b10 b11 b12 b20 b21 b22) =
  MMatrix
    (a00*b00 + a01*b10 + a02*b20)
    (a00*b01 + a01*b11 + a02*b21)
    (a00*b02 + a01*b12 + a02*b22)
    (a10*b00 + a11*b10 + a12*b20)
    (a10*b01 + a11*b11 + a12*b21)
    (a10*b02 + a11*b12 + a12*b22)
    (a20*b00 + a21*b10 + a22*b20)
    (a20*b01 + a21*b11 + a22*b21)
    (a20*b02 + a21*b12 + a22*b22)

x_rot_matrix, y_rot_matrix, z_rot_matrix :: Floating a ⇒ a → MMatrix a

x_rot_matrix a = MMatrix 1 0 0 0 (cos a) (sin a) 0 (- sin a) (cos a)
y_rot_matrix a = MMatrix (cos a) 0 (- sin a) 0 1 0 (sin a) 0 (cos a)
z_rot_matrix a = MMatrix (cos a) (sin a) 0 (- sin a) (cos a) 0 0 0 1

xy_rot_matrix, yx_rot_matrix :: Floating a ⇒ a → a → MMatrix a
  -- rotates over x, then over y. shortcut for (x_rot_matrix x `multMMatrix` y_rot_matrix y)
xy_rot_matrix x y =
  MMatrix (cos y) 0 (- sin y) (sin x * sin y) (cos x) (sin x * cos y) (cos x * sin y) (- sin x) (cos x * cos y)

yx_rot_matrix y x =
  MMatrix (cos y) (sin x * sin y) (cos x * (- sin y)) 0 (cos x) (sin x) (sin y) ((- sin x) * cos y) (cos x * cos y)

x_rot_vector, y_rot_vector :: Floating a ⇒ Vector3 a → a → Vector3 a

x_rot_vector (Vector3 x y z) r = Vector3 x (y * cos r + z * sin r) (y * (- sin r) + z * cos r)
y_rot_vector (Vector3 x y z) r = Vector3 (x * cos r + z * (- sin r)) y (x * sin r + z * cos r)

square :: Num a ⇒ a → a
square x = x * x

wrap :: Ord a ⇒ a → a → a → a
wrap n lower upper = if n < lower then upper else if n > upper then lower else n

dist_sqrd :: Floating a ⇒ Vector3 a → Vector3 a → a
dist_sqrd v w = let d = v <-> w in inner_prod d d

modIntegralPart :: RealFrac r ⇒ r → Integer → r
modIntegralPart (properFraction → (i, f)) n = fromInteger (i `mod` n) + f
