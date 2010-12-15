module Math where

import Prelude hiding ((.))
import MyGL ()
import Graphics.Rendering.OpenGL.GL
import System.Random
import Monad
import MyUtil
import Data.Maybe

-- RANDOM STUFF

newtype RngMonad g d = RngMonad { readRngMonad :: g → (d, g) }

instance (RandomGen g) ⇒ Monad (RngMonad g) where
  (RngMonad f) >>= f' = RngMonad $ \g → let (v, g') = f g in readRngMonad (f' v) g'
  return x = RngMonad $ (,) x

instance (RandomGen g) ⇒ Functor (RngMonad g) where
  fmap f x = x >>= return . f

randomM :: (RandomGen g, Random a) ⇒ RngMonad g a
randomM = RngMonad random

randomRM :: (RandomGen g, Random a) ⇒ (a, a) → RngMonad g a
randomRM = RngMonad . randomR

instance Random a ⇒ Random (Vector3 a) where
  random = readRngMonad $ liftM3 Vector3 randomM randomM randomM
  randomR (Vector3 lx ly lz, Vector3 hx hy hz) = readRngMonad $
    liftM3 Vector3 (randomRM (lx, hx)) (randomRM (ly, hy)) (randomRM (lz, hz))

randomVector3 :: (Random a, Num a, RandomGen g) ⇒ a → RngMonad g (Vector3 a)
randomVector3 size = randomRM (Vector3 (-size) (-size) (-size), Vector3 size size size)

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
tov (Vector3 x y z) = Vertex3 x y z

norm_2 :: Floating a ⇒ Vector3 a → a
norm_2 (Vector3 x y z) = sqrt $ x*x + y*y + z*z

normalize_v :: Floating a ⇒ Vector3 a → Vector3 a
normalize_v v = v </> (norm_2 v)

cross_prod :: Num a ⇒ Vector3 a → Vector3 a → Vector3 a
cross_prod (Vector3 x y z) (Vector3 x' y' z') =
  Vector3 ((y * z') - (z * y')) ((z * x') - (x * z')) ((x * y') - (y * x'))

inner_prod :: Floating a ⇒ Vector3 a → Vector3 a → a
inner_prod (Vector3 x y z) (Vector3 x' y' z') = x*x' + y*y' + z*z'

{-# INLINE inner_prod #-}

data AnnotatedTriangle = AnnotatedTriangle
  { obs_normal :: !V
  , obs_vertices :: !(V, V, V)
    -- TODO: can't we make the V's inside the tuple strict? are tuples strict?
  , obs_projs :: !(V, V, V) -- allow for fast calculation of ray intersections
  , at_center :: !V
  } -- should only ever be constructed using annotate_triangle
  deriving (Show, Read, Eq)

annotate_triangle :: V → V → V → AnnotatedTriangle
annotate_triangle a b c =
  AnnotatedTriangle
    (normalize_v (cross_prod (b <-> a) (c <-> a)))
    (a, b, c)
    (kat a b c, kat b c a, kat c a b)
    ((a <+> b <+> c) </> 3)
  where
   kat a' b' c' = c' <-> ((natob <*> inner_prod natob (c' <-> a')) <+> a')
    where natob = normalize_v (b' <-> a')

ray_triangle_intersection ::
  AnnotatedTriangle → V → V → (GLdouble → V → Bool) → Maybe (GLdouble, V)
ray_triangle_intersection (AnnotatedTriangle n (a, b, c) (anp, bp, cp) _) ray_orig ray_dir inter_pred = do
  eta ← ray_plane_intersection n a ray_orig ray_dir
  let coll = ray_orig <+> (ray_dir <*> eta)
  if eta < 0 || (not $ inter_pred eta coll) ||
    inner_prod anp (coll <-> a) < 0 ||
    inner_prod bp (coll <-> b) < 0 ||
    inner_prod cp (coll <-> c) < 0
   then Nothing else Just (eta, coll)

triangle_collision :: [AnnotatedTriangle] →
  V → V → (GLdouble → V → Bool) → Maybe (GLdouble, V, AnnotatedTriangle)
triangle_collision triangles ray_base ray_dir inter_pred =
  foldr (\o r →
    maybe r (\(eta, coll) →
      if {-inter_pred eta coll &&-} (maybe True (\(oldeta, _, _) → oldeta > eta) r) then Just (eta, coll, o) else r)
     (ray_triangle_intersection o ray_base ray_dir inter_pred)
  ) Nothing triangles

ray_plane_intersection :: V → V → V → V → Maybe GLdouble
ray_plane_intersection plane_normal plane_point ray_orig ray_dir =
  let u = inner_prod plane_normal ray_dir in
  if u < 0 then Just $ -(inner_prod plane_normal (ray_orig <-> plane_point)) / u
  else Nothing

triangles_collide :: AnnotatedTriangle → AnnotatedTriangle → Bool
triangles_collide t@(AnnotatedTriangle _ (a, b, c) _ _) t'@(AnnotatedTriangle _ (a', b', c') _ _) =
  or $ isJust .
    [rti t' a (b <-> a) p, rti t' b (c <-> b) p, rti t' c (a <-> c) p,
    rti t a' (b' <-> a') p, rti t b' (c' <-> b') p, rti t c' (a' <-> c') p]
  where rti = ray_triangle_intersection; p = const . (< 1)

data AnnotatedObstacle = AnnotatedObstacle
  { ao_center :: !V
  , ao_triangles :: [AnnotatedTriangle]
  } deriving (Show, Read)

annotate_obstacle :: [AnnotatedTriangle] → AnnotatedObstacle
annotate_obstacle a = AnnotatedObstacle (foldr1 (<+>) (at_center . a) </> realToFrac (length a)) a

behind :: V → AnnotatedTriangle → Bool
behind v (AnnotatedTriangle n (a, _, _) _ _) = inner_prod (v <-> a) n < 0

point_in_obstacle :: AnnotatedObstacle → V → Bool
tri_in_obstacle :: AnnotatedObstacle → AnnotatedTriangle → Bool
obst_obst_collision :: AnnotatedObstacle → AnnotatedObstacle → Bool

point_in_obstacle (AnnotatedObstacle _ a) v = and $ behind v . a
tri_in_obstacle o (AnnotatedTriangle _ (a, b, c) _ _) = or $ point_in_obstacle o . [a, b, c]
obst_obst_collision (AnnotatedObstacle _ a) (AnnotatedObstacle _ b) =
  or [triangles_collide x y | x ← a, y ← b]

at_max_z :: AnnotatedTriangle → GLdouble
at_max_z (AnnotatedTriangle _ (Vector3 _ _ z0, Vector3 _ _ z1, Vector3 _ _ z2) _ _) =
  z0 `max` z1 `max` z2

at_min_y :: AnnotatedTriangle → GLdouble
at_min_y (AnnotatedTriangle _ (Vector3 _ y0 _, Vector3 _ y1 _, Vector3 _ y2 _) _ _) =
  y0 `min` y1 `min` y2

obst_min_y :: AnnotatedObstacle → GLdouble
obst_min_y (AnnotatedObstacle _ a) = minimum $ at_min_y . a

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
