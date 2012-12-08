{-# LANGUAGE RecordWildCards, ViewPatterns, BangPatterns, UnicodeSyntax, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, NamedFieldPuns, PatternGuards, TypeOperators, TypeFamilies, FlexibleContexts #-}

module Math where

import Prelude hiding ((.))
import Graphics.Rendering.OpenGL.GL (Vector3(..), GLdouble, Color3(..), Normal3(..), Color4(..), Vertex3(..))
import Control.Monad
import Util ((.), tupleToList)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Tuple (swap)
import Data.Either (partitionEithers)
import Data.Maybe (isJust, mapMaybe)
import Control.Applicative (liftA3)
import Control.Monad.Random (Random(..), MonadRandom(..), runRand)
import Control.DeepSeq (NFData)
import Foreign.Storable (Storable(..), sizeOf)
import qualified Foreign.Storable.Record as Store
import qualified Data.StorableVector as SV
import Foreign.Storable.Tuple ()
import Data.AdditiveGroup (AdditiveGroup, (^+^), (^-^))
import Data.VectorSpace (VectorSpace, Scalar, (^*), (^/), normalized, magnitudeSq, (<.>), InnerSpace)
import Data.VectorSpace.OpenGL ()
import Data.Cross (HasCross3(cross3))

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

data Matrix33 a = Matrix33 !(Vector3 a) !(Vector3 a) !(Vector3 a) -- three rows
  deriving (Eq, Show, Read)

scalar_matrix_mult :: VectorSpace (Vector3 a) ⇒ Scalar (Vector3 a) → Matrix33 a → Matrix33 a
scalar_matrix_mult x (Matrix33 a b c) = Matrix33 (a ^* x) (b ^* x) (c ^* x)

inv :: (Fractional a, VectorSpace (Vector3 a), Scalar (Vector3 a) ~ a) ⇒ Matrix33 a → Matrix33 a
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

data AnnotatedTriangle = AnnotatedTriangle
  { triangleNormal :: !V
  , triangleVertices :: !(V, V, V)
    -- TODO: can't we make the V's inside the tuple strict? are tuples strict?
  , triangleCenter :: !V
  , toTriangleCoords :: !(Matrix33 GLdouble) -- maps world coordinates to triangle coordinates with (b-a, c-a, normal) as basis
  } -- should only ever be constructed using annotate_triangle
  deriving (Show, Read, Eq)

annotateTriangle :: (GLdouble ~ Scalar (Vector3 GLdouble)) => V → V → V → AnnotatedTriangle
annotateTriangle a b c = AnnotatedTriangle{..}
  where
    triangleNormal@(Vector3 nx ny nz) = normalized (cross3 atob atoc)
    triangleVertices = (a, b, c)
    triangleCenter = (a ^+^ b ^+^ c) ^/ 3
    atob@(Vector3 bx by bz) = (b ^-^ a)
    atoc@(Vector3 cx cy cz) = (c ^-^ a)
    toTriangleCoords = inv $ Matrix33 (Vector3 bx cx nx) (Vector3 by cy ny) (Vector3 bz cz nz)

newtype OriginRay = OriginRay V

data Ray = Ray { rayOrigin, rayDirection :: !V } deriving (Read, Show)
data Plane = Plane { planeNormal, planePoint :: !V }
data Sphere = Sphere { sphereCenter :: !V, sphereSquaredRadius :: !GLdouble } deriving (Read, Show)
data Cube = Cube { cubeLoCorner, cubeHiCorner :: !(Vector3 GLdouble) } deriving (Read, Show)
  -- Cube invariant: components of lo <= hi

instance NFData Cube

plane :: AnnotatedTriangle → Plane
plane (AnnotatedTriangle n (a, _, _) _ _) = Plane n a

class FitsIn o c where fitsIn :: o → c → Bool

instance Vector3 GLdouble `FitsIn` Cube where
  fitsIn !(Vector3 x y z) !(Cube (Vector3 a b c) (Vector3 a' b' c')) =
    a <= x && x <= a' &&
    b <= y && y <= b' &&
    c <= z && z <= c'

instance Sphere `FitsIn` Cube where
  fitsIn !Sphere{..} c =
    fitsIn (sphereCenter ^+^ Vector3 radius radius radius) c &&
    fitsIn (sphereCenter ^-^ Vector3 radius radius radius) c
    where radius = sqrt sphereSquaredRadius

instance GeometricObstacle `FitsIn` Cube where
  fitsIn = fitsIn . obstacleSphere

sortPair :: Ord a ⇒ (a, a) → (a, a)
sortPair !p@(x, y)
  | x <= y = p
  | otherwise = (y, x)

inRange :: Ord a ⇒ a → Range a → Bool
inRange x !(Range a b) = a <= x && x <= b

st :: (Fractional a, Ord a) ⇒ a → a → Range a → Either Bool (a, a)
st !p !v !range@Range{..}
  | abs v <= 0.0001 = Left $ inRange p range
  | otherwise = Right $ sortPair ((lo - p) / v, (hi - p) / v)

data Range a = Range { lo, hi :: !a } -- invariant: lo <= hi

class Collision a b c | a b → c where
  collision :: a → b → c
  collide :: a → b → Bool

instance Ord a ⇒ Collision (Range a) (Range a) (Maybe (Range a)) where
  collide a b = isJust (collision a b)
  collision !(Range lo hi) !(Range lo' hi')
    | lo'' <= hi'' = Just $ Range lo'' hi''
    | otherwise = Nothing
    where r@(lo'', hi'') = (max lo lo', min hi hi')

instance Collision Cube Cube Bool where -- should really be done for cuboids with a cuboid result
  collide = collision
  collision (Cube (Vector3 x y z) (Vector3 a b c)) (Cube (Vector3 x' y' z') (Vector3 a' b' c')) =
    collide (Range x a) (Range x' a') &&
    collide (Range y b) (Range y' b') &&
    collide (Range z c) (Range z' c')

instance Collision (Vector3 GLdouble) Cube Bool where
  collide = collision
  collision !(Vector3 x y z) !(Cube (Vector3 a b c) (Vector3 a' b' c')) =
    inRange x (Range a a') &&
    inRange y (Range b b') &&
    inRange z (Range c c')

instance Collision Ray Cube Bool where
  collide = collision
  collision
    !Ray{rayOrigin=rayOrigin@(Vector3 rox roy roz), rayDirection=rayDirection@(Vector3 rdx rdy rdz)}
    !cube@(Cube (Vector3 ccx ccy ccz) (Vector3 cex cey cez)) =
      not wayOff && (allInside || (and bs && (null rs || maximum (0 : map fst rs) <= minimum (1 : map snd rs))))
    where
      wayOff =
        (rox <= ccx && rex <= ccx) || (cex <= rox && cex <= rex) ||
        (roy <= ccy && rey <= ccy) || (cey <= roy && cey <= rey) ||
        (roz <= ccz && rez <= ccz) || (cez <= roz && cez <= rez)
      allInside = collision rayOrigin cube && collision (rayOrigin ^+^ rayDirection) cube
      rayEnd@(Vector3 rex rey rez) = rayOrigin ^+^ rayDirection
      --cubeOppCorner@(Vector3 cex cey cez) = Vector3 (ccx + cubeSize) (ccy + cubeSize) (ccz + cubeSize)
      (bs, rs) = partitionEithers [iX, iY, iZ]
      rs' = (0, 1) : rs
      iX = st rox rdx (Range ccx cex)
      iY = st roy rdy (Range ccy cey)
      iZ = st roz rdz (Range ccz cez)

instance Collision Ray Sphere Bool where
  collide = collision
  collision !Ray{..} !Sphere{..} = b*b - 4*a*c >= 0
    where
      a = magnitudeSq rayDirection
      b = 2 * (rayDirection <.> o)
      c = magnitudeSq o - sphereSquaredRadius
      o = rayOrigin ^-^ sphereCenter

sameDirection :: V → V → Bool
sameDirection a b = (a <.> b) >= 0

instance Collision Ray AnnotatedTriangle (Maybe GLdouble) where
  collide a b = isJust $ collision a b
  collision !Ray{..} !AnnotatedTriangle{..} = do
    let
      (a, _, _) = triangleVertices
      Vector3 h i j = matrix_vector_mult toTriangleCoords (rayOrigin ^-^ a)
      Vector3 k l m = matrix_vector_mult toTriangleCoords rayDirection
      eta = - j / m
      v = h + k * eta
      w = i + l * eta
    guard $ 0 <= eta && eta <= 1 && 0 <= v && 0 <= w && v + w <= 1
    return eta

matrix_vector_mult :: InnerSpace t ⇒ Matrix33 t → Vector3 t → Vector3 (Scalar t)
matrix_vector_mult !(Matrix33 a b c) d = Vector3 (a <.> d) (b <.> d) (c <.> d)

instance Collision Ray [AnnotatedTriangle] (Maybe (GLdouble, AnnotatedTriangle)) where
  collide a b = isJust $ collision a b
  collision ray triangles
      | null collisions = Nothing
      | otherwise = Just $ minimumBy (compare `on` fst) collisions
    where
      collisions :: [(GLdouble, AnnotatedTriangle)]
      collisions = swap . mapMaybe (\triangle → (,) triangle . (ray `collision` triangle)) triangles

data StoredVertex = StoredVertex
  { storedPosition, storedNormal :: !(Vector3 GLdouble), storedColor :: !(Color3 GLdouble) }

instance Storable StoredVertex where
  sizeOf = Store.sizeOf storeVertex
  alignment = Store.alignment storeVertex
  peek = Store.peek storeVertex
  poke = Store.poke storeVertex

storeVertex :: Store.Dictionary StoredVertex
storeVertex = Store.run $ liftA3 StoredVertex
  (Store.element storedPosition)
  (Store.element storedNormal)
  (Store.element storedColor)

asStoredVertices :: [VisualObstacle] → SV.Vector StoredVertex
asStoredVertices obstacles = SV.pack $
  [ StoredVertex vertex triangleNormal obstacleColor
  | VisualObstacle{..} ← obstacles
  , AnnotatedTriangle{..} ← obstacleTriangles geometricObstacle
  , vertex ← tupleToList triangleVertices ]

rayThrough :: V → V → Ray
rayThrough from to = Ray from (to ^-^ from)

lineLoop :: [a] → [(a, a)]
lineLoop [] = []
lineLoop (x:xs) = go x xs
  where
    go l [] = [(l, x)]
    go l (y:ys) = (l, y) : go y ys

instance Collision AnnotatedTriangle AnnotatedTriangle Bool where
  collide = collision
  collision t t' = or $
      h t' . lineLoop (tupleToList $ triangleVertices t) ++ h t . lineLoop (tupleToList $ triangleVertices t')
    where
      h :: AnnotatedTriangle → (V, V) → Bool
      h u (x, y) = rayThrough x y `collide` u

data GeometricObstacle = GeometricObstacle
  { obstacleSphere :: !Sphere
  , obstacleTriangles :: ![AnnotatedTriangle]
  } deriving (Show, Read)

triangleCube :: AnnotatedTriangle -> Cube
triangleCube AnnotatedTriangle{triangleVertices=(Vector3 x y z, Vector3 x' y' z', Vector3 x'' y'' z'')} =
  Cube
    (Vector3 (x `min` x' `min` x'') (y `min` y' `min` y'') (z `min` z' `min` z''))
    (Vector3 (x `max` x' `max` x'') (y `max` y' `max` y'') (z `max` z' `max` z''))

enclosingCube :: Cube -> Cube -> Cube
enclosingCube (Cube (Vector3 x y z) (Vector3 a b c)) (Cube (Vector3 x' y' z') (Vector3 a' b' c')) =
  Cube (Vector3 (min x x') (min y y') (min z z')) (Vector3 (max a a') (max b b') (max c c'))

obstacleCube :: GeometricObstacle → Cube
obstacleCube GeometricObstacle{obstacleTriangles} =
  foldr1 enclosingCube (map triangleCube obstacleTriangles)
    -- todo: different fold maybe

instance Collision GeometricObstacle Cube Bool where
  collide = collision
  collision = collision . obstacleCube

bytesPerVertex, bytesPerDouble, bytesPerVector, bytesPerObstacle, bytesPerTriangle, verticesPerObstacle, trianglesPerObstacle, verticesPerTriangle :: Num a ⇒ a
trianglesPerObstacle = 4
verticesPerTriangle = 3
verticesPerObstacle = trianglesPerObstacle * verticesPerTriangle
bytesPerObstacle = trianglesPerObstacle * bytesPerTriangle
bytesPerTriangle = verticesPerTriangle * bytesPerVertex
bytesPerVertex = fromIntegral $ sizeOf (undefined :: StoredVertex)
bytesPerDouble = fromIntegral $ sizeOf (undefined :: Double)
bytesPerVector = fromIntegral $ sizeOf (undefined :: Vector3 GLdouble)


instance NFData GeometricObstacle

data VisualObstacle = VisualObstacle
  { obstacleColor :: !(Color3 GLdouble)
  , geometricObstacle :: !GeometricObstacle }

annotateObstacle :: [AnnotatedTriangle] → GeometricObstacle
annotateObstacle obstacleTriangles = GeometricObstacle{obstacleSphere=Sphere{..}, ..}
  where
    sphereCenter = foldr1 (^+^) (triangleCenter . obstacleTriangles) ^/ realToFrac (length obstacleTriangles)
    sphereSquaredRadius = maximum $ magnitudeSq . (^-^ sphereCenter) . (obstacleTriangles >>= tupleToList . triangleVertices)

behind :: V → Plane → Bool
behind v Plane{..} = sameDirection (planePoint ^-^ v) planeNormal

point_in_obstacle :: GeometricObstacle → V → Bool
point_in_obstacle o v = and $ behind v . plane . obstacleTriangles o

tri_in_obstacle :: GeometricObstacle → AnnotatedTriangle → Bool
tri_in_obstacle o = or . (point_in_obstacle o .) .  tupleToList . triangleVertices

instance Collision GeometricObstacle GeometricObstacle Bool where
  collide = collision
  collision a b = or [x `collision` y | x ← obstacleTriangles a, y ← obstacleTriangles b]

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

wrap :: Ord a ⇒ a → a → a → a
wrap n lower upper = if n < lower then upper else if n > upper then lower else n

modIntegralPart :: RealFrac r ⇒ r → Integer → r
modIntegralPart (properFraction → (i, f)) n = fromInteger (i `mod` n) + f

randomAngle :: (MonadRandom m, Floating f, Random f) ⇒ m f
randomAngle = getRandomR (0, 2 * pi)

unitCirclePoint :: GLdouble → V
unitCirclePoint a = Vector3 (sin a) 0 (cos a)
