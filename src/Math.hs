{-# LANGUAGE RecordWildCards, ViewPatterns, BangPatterns, UnicodeSyntax, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, NamedFieldPuns #-}

module Math where

import Prelude hiding ((.))
import MyGL ()
import Graphics.Rendering.OpenGL.GL (Vector3(..), GLdouble, Color3(..), Normal3(..), Color4(..), Vertex3(..))
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

instance Random a ⇒ Random (Color3 a) where
  random = runRand $ liftM3 Color3 getRandom getRandom getRandom
  randomR (Color3 lx ly lz, Color3 hx hy hz) = runRand $
    liftM3 Color3 (getRandomR (lx, hx)) (getRandomR (ly, hy)) (getRandomR (lz, hz))

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

data Matrix33 a = Matrix33 !(Vector3 a) !(Vector3 a) !(Vector3 a) -- three rows
  deriving (Eq, Show, Read)

scalar_matrix_mult :: Num a => a -> Matrix33 a -> Matrix33 a
scalar_matrix_mult x (Matrix33 a b c) = Matrix33 (a <*> x) (b <*> x) (c <*> x)

inv :: Fractional a => Matrix33 a -> Matrix33 a
inv (Matrix33 (Vector3 a b c) (Vector3 d e f) (Vector3 g h k)) =
    scalar_matrix_mult (1 / det) (Matrix33 (Vector3 a' d' g') (Vector3 b' e' h') (Vector3 c' f' k'))
  where
    det = a * (e * k - f * h) - b * (k * d - f * g) + c * (d * h - e * g)
    a' = e * k - f * h
    b' = f * g - d * k
    c' = d * h - e * g
    d' = c * h - b * k
    e' = a * k - c * g
    f' = g * b - a * h
    g' = b * f - c * e
    h' = c * d - a * f
    k' = a * e - b * d
  -- Copied from Wikipedia

tov :: Vector3 a → Vertex3 a
tov (Vector3 !x !y !z) = Vertex3 x y z

vectorToNormal :: Vector3 a → Normal3 a
vectorToNormal (Vector3 !x !y !z) = Normal3 x y z

norm_2 :: Floating a ⇒ Vector3 a → a
norm_2 (Vector3 !x !y !z) = sqrt $ x*x + y*y + z*z

normalize_v :: Floating a ⇒ Vector3 a → Vector3 a
normalize_v v = v </> norm_2 v

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
  , triangleCenter :: !V
  , toTriangleCoords :: !(Matrix33 GLdouble) -- maps world coordinates to triangle coordinates with (b-a, c-a, normal) as basis
  } -- should only ever be constructed using annotate_triangle
  deriving (Show, Read, Eq)

annotateTriangle :: V → V → V → AnnotatedTriangle
annotateTriangle a b c = AnnotatedTriangle{..}
  where
    triangleNormal@(Vector3 nx ny nz) = normalize_v (cross_prod atob atoc)
    triangleVertices = (a, b, c)
    triangleCenter = (a <+> b <+> c) </> 3
    atob@(Vector3 bx by bz) = (b <-> a)
    atoc@(Vector3 cx cy cz) = (c <-> a)
    toTriangleCoords = inv $ Matrix33 (Vector3 bx cx nx) (Vector3 by cy ny) (Vector3 bz cz nz)

newtype OriginRay = OriginRay V

data Ray = Ray { rayOrigin, rayDirection :: !V } deriving (Read, Show)
data Plane = Plane { planeNormal, planePoint :: !V }
data Sphere = Sphere { sphereCenter :: !V, sphereSquaredRadius :: !GLdouble } deriving (Read, Show)
data Cube = Cube { cubeCorner :: Vector3 Int, cubeSize :: Int } deriving (Read, Show)

plane :: AnnotatedTriangle → Plane
plane (AnnotatedTriangle n (a, _, _) _ _) = Plane n a

class FitsInCube o where fitsInCube :: o -> Cube -> Bool

instance FitsInCube (Vector3 GLdouble) where
  fitsInCube (Vector3 x y z) (Cube (Vector3 (fromIntegral -> a) (fromIntegral -> b) (fromIntegral -> c)) (fromIntegral -> cubeSize)) =
    a <= x && x <= a + cubeSize &&
    b <= y && y <= b + cubeSize &&
    c <= z && z <= c + cubeSize
      -- todo: this is fugly

instance FitsInCube Sphere where
  fitsInCube Sphere{..} c =
    fitsInCube (sphereCenter <+> Vector3 radius radius radius) c &&
    fitsInCube (sphereCenter <-> Vector3 radius radius radius) c
    where radius = sqrt sphereSquaredRadius

instance FitsInCube GeometricObstacle where
  fitsInCube = fitsInCube . obstacleSphere

sortPair :: Ord a => (a, a) -> (a, a)
sortPair (x, y) = (min x y, max x y)

inRange :: Ord a => a -> Range a -> Bool
inRange x (a, b) = a <= x && x <= b

st :: (Fractional a, Ord a) => a -> a -> Range a -> Either Bool (a, a)
st p v range@(lo, hi)
  | abs v <= 0.0001 = Left $ inRange p range
  | otherwise = Right $ sortPair ((lo - p) / v, (hi - p) / v)

data Dimension = X | Y | Z

component :: Dimension -> Vector3 a -> a
component X (Vector3 v _ _) = v
component Y (Vector3 _ v _) = v
component Z (Vector3 _ _ v) = v

type Range a = (a, a) -- invariant: fst <= snd

intersectRanges :: Ord a => Range a -> Range a -> Maybe (Range a)
intersectRanges (lo, hi) (lo', hi')
  | lo'' <= hi'' = Just r
  | otherwise = Nothing
  where r@(lo'', hi'') = (max lo lo', min hi hi')


class Collision a b c | a b → c where collision :: a → b → c

instance Collision Ray Cube Bool where
  collision Ray{..} Cube{..} = foldl f (Right (0, 1)) (map i [X, Y, Z]) /= Left False
    where
      i :: Dimension -> Either Bool (Range GLdouble)
      i d = st (component d rayOrigin) (component d rayDirection)
          (fromIntegral $ component d cubeCorner, fromIntegral $ component d cubeCorner + cubeSize)
      f (Left True) b = b
      f a@(Left False) _ = a
      f a (Left True) = a
      f _ b@(Left False) = b
      f (Right r) (Right r') = case intersectRanges r r' of
        Just r'' -> Right r''
        Nothing -> Left False

instance Collision Ray Sphere Bool where
  collision !Ray{..} Sphere{..} = b*b - 4*a*c >= 0
    where
      a = inner_prod rayDirection rayDirection
      b = 2 * inner_prod rayDirection o
      c = inner_prod o o - sphereSquaredRadius
      o = rayOrigin <-> sphereCenter

sameDirection :: V → V → Bool
sameDirection a b = inner_prod a b >= 0

instance Collision (Ray, GLdouble → V → Bool) AnnotatedTriangle (Maybe (GLdouble, V)) where
  collision !(Ray{..}, inter_pred) !AnnotatedTriangle{..} = do
    let
      (a, _, _) = triangleVertices
      Vector3 h i j = matrix_vector_mult toTriangleCoords (rayOrigin <-> a)
      Vector3 k l m = matrix_vector_mult toTriangleCoords rayDirection
      coll = rayOrigin <+> (rayDirection <*> eta)
      eta = - j / m
      v = h + k * eta
      w = i + l * eta
    guard $ eta >= 0 && inter_pred eta coll && v >= 0 && w >= 0 && v + w <= 1
    return (eta, coll)
      -- The predicate is integrated because applying it after-the-fact is more expensive, because by that time the three inner_prods have already been evaluated.

matrix_vector_mult :: Num a => Matrix33 a -> Vector3 a -> Vector3 a
matrix_vector_mult !(Matrix33 a b c) d = Vector3 (inner_prod a d) (inner_prod b d) (inner_prod c d)

instance Collision (Ray, GLdouble → V → Bool) [AnnotatedTriangle] (Maybe (GLdouble, V, AnnotatedTriangle)) where
  collision ray triangles
      | null collisions = Nothing
      | otherwise = Just $ minimumBy (compare `on` (\(_,x,_) → x)) collisions
    where
      collisions :: [(GLdouble, V, AnnotatedTriangle)]
      collisions = (\(x,(y,z)) → (y,z,x)) . mapMaybe (\triangle → (,) triangle . (ray `collision` triangle)) triangles

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
  , obstacleTriangles :: ![AnnotatedTriangle]
  } deriving (Show, Read)

data VisualObstacle = VisualObstacle
  { geometricObstacle :: !GeometricObstacle
  , obstacleColor :: !(Color3 GLdouble) }

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

data MMatrix a = MMatrix !a !a !a !a !a !a !a !a !a deriving Show

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
