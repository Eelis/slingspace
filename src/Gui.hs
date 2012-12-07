{-# LANGUAGE RecordWildCards, ViewPatterns, UnicodeSyntax, ScopedTypeVariables, PatternGuards, NamedFieldPuns, DeriveDataTypeable, MultiWayIf, FlexibleInstances #-}

module Gui (Controller(..), Scheme(..), GuiConfig(..), GunGuiConfig(..), FloorConfig(..), GridType(..), CameraConfig(..), gui, players) where

import Data.Map (Map)
import Graphics.Rendering.OpenGL.GL (Vector3(..), GLdouble, ($=), Vertex3(..), Vertex4(..), vertex, PrimitiveMode(..), GLfloat, Color4(..), GLclampf, ClearBuffer(..), Face(..), Capability(..), hint, renderPrimitive, lighting, ColorMaterialParameter(AmbientAndDiffuse), MatrixComponent, rotate)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Math (V, (<+>), (<->), (<*>), x_rot_vector, y_rot_vector, tov, normalize_v, Ray(..), Cube(..), trianglesPerObstacle, verticesPerTriangle, StoredVertex, bytesPerObstacle, verticesPerObstacle)
import Data.Maybe (isJust, mapMaybe)
import Control.Monad (when, forM_, unless)
import Data.Traversable (forM)
import Control.Monad.Fix (fix)
import Logic (Player(Player,body), Gun(..), Rope(..), findTarget, Life(..), positions, birth, GunConfig(shootingRange))
import Util ((.), getDataFileName, whenJust, loadConfig)
import Prelude hiding ((.), mapM)
import Control.Monad.Reader (ReaderT(..), ask, asks, lift)
import Foreign.Ptr (nullPtr, plusPtr, Ptr)
import Foreign.Storable (sizeOf)
import Data.Traversable (mapM)
import Control.DeepSeq (deepseq)
import Obstacles (ObstacleTree)
import Data.Typeable (Typeable)
import Controllers (Controller(..), players)

import qualified Octree
import qualified Logic
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Pointer as SVP
import qualified Data.Map as Map
import qualified Graphics.UI.GLFW as GLFW
import qualified Control.Monad.State as CMS
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.GLU.Raw as GLU

-- Static data:

data Scheme = Scheme
  { shadow_color :: Color4 GLclampf
  , grid_color :: Color4 GLclampf
  , rope_line_width :: GLfloat
  , fog_density :: GLfloat
  , fog_color :: Color4 GLclampf
  , gunColor :: Gun -> Color4 GLclampf
  , lightModel_ambient
  , ballLight_ambient, ballLight_diffuse
  , ball_material_ambient, ball_material_diffuse :: Color4 GLfloat
  , ballLight_attenuation :: (GLfloat, GLfloat, GLfloat)
  } deriving Typeable

data GridType =
  DottedGrid { grid_dot_size :: GLfloat } |
  LinedGrid { grid_line_width :: GLfloat }

data FloorConfig = Grid { grid_size :: Integer, grid_type :: GridType } {-| Shadows-}

data CameraConfig = CameraConfig
  { viewing_dist :: GLdouble
  , fov :: GLdouble -- in degrees
  , wheelBounds :: (Int ,Int)
  , zoom :: Int → GLdouble
  , mouse_speed :: GLdouble -- in pixels per radian
  , invert_mouse :: Bool
  }

data GunGuiConfig = GunGuiConfig { gun_xrot, gun_yrot :: GLdouble {- in radians -} }

data GuiConfig = GuiConfig
  { windowTitle :: String
  , gunGuiConfig :: Gun -> GunGuiConfig
  , ugly :: Bool
  , floorConf :: Maybe FloorConfig
  , playerSize :: GLdouble
  , camConf :: CameraConfig
  , schemeFile :: String
  , restart_key, pause_key, exit_key :: GLFW.Key
  , gunForButton :: GLFW.MouseButton → Maybe Gun
  , tickDuration :: Double -- in seconds
  } deriving Typeable

data Static = Static
  { obstacleBuffer :: GL.BufferObject
  , bodyQuadric :: Ptr GLU.GLUquadric
  , scheme :: Scheme
  , guiConfig :: GuiConfig
  , gunConfig :: Gun -> GunConfig -- not part of GuiConfig because gunConfig is normally read from a gameplay config file
  , obstacleCount :: Int
  , tree :: ObstacleTree }

type Gui = ReaderT Static IO

-- Dynamic data:

data CameraOrientation = CameraOrientation { cam_dist, cam_xrot, cam_yrot :: !GLdouble }
data FireState = FireAsap | ReleaseAsap | Fired | Idle
data ClientGunState = ClientGunState { target :: Maybe V, fireState :: FireState }
type Guns = Map Gun ClientGunState

data State c = State
  { controller :: c
  , paused :: Bool
  , camera :: CameraOrientation
  , guns :: Guns }



rotateRadians :: (Floating c, MatrixComponent c) ⇒ c → Vector3 c → IO ()
rotateRadians r = rotate (r / pi * 180)

green :: Color4 GLclampf
green = Color4 0 1 0 1

initialGuns :: Guns
initialGuns = Map.fromList $ flip (,) (ClientGunState Nothing Idle) . [LeftGun, RightGun]

drawEverything :: Controller c => State c → Gui ()
drawEverything State{camera=CameraOrientation{..}, ..} = do
  lift $ do
    GL.clear [ColorBuffer, DepthBuffer]
    GL.loadIdentity
    GL.translate $ Vector3 0 0 (- cam_dist)
    lighting $= Enabled
    GL.position ballLight $= Vertex4 30 30 100 1
    rotateRadians cam_xrot $ Vector3 1 0 0
    rotateRadians cam_yrot $ Vector3 0 1 0

  whenJust (player controller >>= birth) $ \me → do
  lift $ GL.translate $ (rayOrigin $ body me) <*> (-1)
  drawPlayers $ mapMaybe birth $ players controller
  drawObstacles
  lift $ lighting $= Disabled
  --lift $ drawFutures players
  --drawTree tree
  drawFloor {-(shootableObstacles >>= obstacleTriangles)-} me
  drawRopes $ mapMaybe birth $ players controller
  --drawOrientation (head . players)
  --drawSectorBorders $ head $ head $ Map.elems players
  drawCrossHairs guns


onReshape :: CameraConfig → GLFW.WindowSizeCallback
onReshape CameraConfig{..} w h = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GLU.gluPerspective fov (fromIntegral w / fromIntegral h) 10 viewing_dist
  GL.matrixMode $= GL.Modelview 0

onKey :: GuiConfig → GLFW.Key → State c → State c
onKey
    GuiConfig{camConf=CameraConfig{..}, ..}
    k state@State{..}
  | k == pause_key = state{paused=not paused}
  | otherwise = state

keyCallback :: GuiConfig → IORef (State c) → GLFW.KeyCallback
keyCallback guiConfig@GuiConfig{camConf=CameraConfig{..}} stateRef k b = when b $ do
  let
    updateOrientation x y c@CameraOrientation{..} = c{
      cam_xrot = cam_xrot + (if invert_mouse then negate else id) (fromIntegral y / mouse_speed),
      cam_yrot = cam_yrot + fromIntegral x / mouse_speed }
  s ← readIORef stateRef
  let s' = onKey guiConfig k s
  writeIORef stateRef s'
  when (paused s /= paused s') $
    if paused s'
      then do
        GLFW.setMousePositionCallback $ \_ _ → return ()
        GLFW.enableMouseCursor
      else do
        GLFW.disableMouseCursor
        r ← GLFW.getMousePosition >>= newIORef
        GLFW.setMousePositionCallback $ \x' y' → do
          (x, y) ← readIORef r
          writeIORef r (x', y')
          modifyIORef' stateRef $ \st → st{camera=updateOrientation (x' - x) (y' - y) (camera st)}

glfwLoop :: Controller c => State c → Gui (State c)
glfwLoop initialState = do
  context@Static
    {guiConfig=guiConfig@GuiConfig{camConf=camConf@CameraConfig{..}, ..}, ..} ← ask
  lift $ do

  stateRef ← newIORef initialState

  GLFW.setWindowBufferSwapInterval 1

  GLFW.setWindowSizeCallback (onReshape camConf)
  GLFW.setKeyCallback $ keyCallback guiConfig stateRef
  GLFW.setMouseWheelCallback $ \p → let (l, u) = wheelBounds in if
      | p < l → GLFW.setMouseWheel l
      | p > u → GLFW.setMouseWheel u
      | otherwise → modifyIORef' stateRef $ \s → s{camera=(camera s){ cam_dist = zoom p } }
  GLFW.setMouseButtonCallback $ \but b → case gunForButton but of
    Just g → modifyIORef' stateRef $ \s →
      s{guns=Map.adjust (\gs → gs { fireState = if b then FireAsap else ReleaseAsap }) g (guns s)}
    Nothing → return ()

  closeRef ← newIORef False
  GLFW.setWindowCloseCallback $ writeIORef closeRef True >> return True

  GLFW.resetTime
  flip fix 0 $ \loop t → do
    n ← GLFW.getTime
    state ← readIORef stateRef
    if n > t
      then do
        s ← runReaderT (guiTick state) context
        writeIORef stateRef s
        loop $ t + tickDuration
      else do
        runReaderT (drawEverything state) context
        GLFW.swapBuffers
        GLFW.pollEvents
        b ← GLFW.keyIsPressed exit_key
        c ← readIORef closeRef
        unless (b || c) $ loop t

  readIORef stateRef


-- Drawers:

ballLight :: GL.Light
ballLight = GL.Light 0

drawFloor :: {-[AnnotatedTriangle] →-} Player → Gui ()
drawFloor {-visible_obs-} Player{..} = do
  Scheme{..} ← asks scheme
  GuiConfig{camConf=CameraConfig{..}, ..} ← asks guiConfig
  lift $ do
  whenJust floorConf $ \kind → do
  case kind of
{-
    Shadows → do
      GLUT.color shadow_color
      GLUT.renderPrimitive Triangles $ forM_ visible_obs $
        mapM (vertex . tov . toFloor) . tupleToList . triangleVertices
-}
    Grid{..} → do
      GL.color grid_color
      let
        Vector3 x _ z = rayOrigin body
        funky h = fromInteger (h' - (h' `mod` grid_size)) :: GLdouble where h' = round h :: Integer
        aligned_z = funky z; aligned_x = funky x
        vd = viewing_dist
      case grid_type of
        LinedGrid{..} → do
          GL.lineWidth $= grid_line_width
          renderPrimitive Lines $
            forM_ [-vd, -vd + (fromInteger grid_size) .. vd] $ \n →
              mapM (vertex . tov) $
                [ Vector3 (aligned_x + n) 0 (z - vd), Vector3 (aligned_x + n) 0 (z + vd)
                , Vector3 (x - vd) 0 (aligned_z + n), Vector3 (x + vd) 0 (aligned_z + n) ]
        DottedGrid{..} → do
          GL.pointSize $= grid_dot_size
          GL.renderPrimitive Points $
            forM_ [(aligned_x + x', aligned_z + z') | x' ← [-vd, -vd + (fromInteger grid_size) .. vd], z' ← [-vd, -vd + (fromInteger grid_size) .. vd]] $ \(x', z') →
              vertex $ tov $ Vector3 x' 0 z'

drawTree :: ObstacleTree -> Gui ()
drawTree = lift . renderPrimitive Lines . go Nothing
  where
    go :: Maybe V -> ObstacleTree -> IO ()
    go mp t = do
      let
        s = cubeSize (fst t) / 2 :: GLdouble
        center = cubeCorner (fst t) <+> Vector3 s s s
      forM_ (Octree.subs t) (go (Just center))
      case mp of
        Nothing -> return ()
        Just p -> mapM_ (vertex . tov) [center, p]

drawCrossHairs :: Guns → Gui ()
drawCrossHairs guns = do
  scheme ← asks scheme
  guiConfig ← asks guiConfig
  lift $ do
  GL.lineWidth $= 3
  GL.pointSize $= 4
  forM_ (Map.toList guns) $ \(g, gu) → do
    let GunGuiConfig{..} = gunGuiConfig guiConfig g
    GL.color $ gunColor scheme g
    GL.loadIdentity
    rotateRadians gun_xrot $ Vector3 (-1) 0 0
    rotateRadians gun_yrot $ Vector3 0 (-1) 0
    GL.renderPrimitive Points $ vertex $ Vertex3 (0 :: GLdouble) 0 (-100)
    when (isJust $ target gu) $ GL.renderPrimitive LineLoop $ mapM_ vertex
      [ Vertex3 (-1 :: GLdouble) 0 (-100)
      , Vertex3 (0 :: GLdouble) (-1) (-100)
      , Vertex3 (1 :: GLdouble) 0 (-100)
      , Vertex3 (0 :: GLdouble) 1 (-100) ]

drawRopes :: [Player] → Gui ()
drawRopes ps = do
  Scheme{..} ← asks scheme
  GuiConfig{..} ← asks guiConfig
  lift $ do
  GL.lineWidth $= rope_line_width
  renderPrimitive Lines $ forM ps $ \p@Player{..} →
    forM_ (Map.toList (Logic.guns p)) $ \(gun, Rope{..}) → do
        GL.color $ gunColor gun
        vertex $ tov $ rayOrigin body <+> (normalize_v (rayOrigin rope_ray <-> rayOrigin body) <*> (playerSize + 0.05))
        vertex $ tov $ rayOrigin rope_ray
  return ()

drawOrientation :: Map String Player → Gui ()
drawOrientation p = do
  Scheme{..} ← asks scheme
  GuiConfig{..} ← asks guiConfig
  lift $ do
  GL.lineWidth $= rope_line_width
  renderPrimitive Lines $ forM p $ \Player{..} → do
    vertex $ tov $ rayOrigin body <+> Vector3 (-100) 0 0
    vertex $ tov $ rayOrigin body <+> Vector3 100 0 0
    vertex $ tov $ rayOrigin body <+> Vector3 0 0 (-100)
    vertex $ tov $ rayOrigin body <+> Vector3 0 0 100
  return ()

drawObstacles :: Gui ()
drawObstacles = do
  Static{scheme=Scheme{..}, ..} ← ask
  lift $ do

  GL.materialDiffuse Front $= Color4 1 1 1 1
  GL.materialAmbient Front $= Color4 0.4 0.6 0.8 1
  GL.clientState GL.VertexArray $= Enabled
  GL.clientState GL.NormalArray $= Enabled
  GL.clientState GL.ColorArray $= Enabled
  GL.bindBuffer GL.ArrayBuffer $= Just obstacleBuffer
  let bytesPerVertex = fromIntegral $ sizeOf (undefined :: StoredVertex)
  let bytesPerVector = fromIntegral $ sizeOf (undefined :: Vector3 GLdouble)
  GL.arrayPointer GL.VertexArray
    $= GL.VertexArrayDescriptor 3 GL.Double bytesPerVertex (plusPtr nullPtr (0 * bytesPerVector))
  GL.arrayPointer GL.NormalArray
    $= GL.VertexArrayDescriptor 3 GL.Double bytesPerVertex (plusPtr nullPtr (1 * bytesPerVector))
  GL.arrayPointer GL.ColorArray
    $= GL.VertexArrayDescriptor 3 GL.Double bytesPerVertex (plusPtr nullPtr (2 * bytesPerVector))

  let totalVertices = obstacleCount * trianglesPerObstacle * verticesPerTriangle
  GL.drawArrays Triangles 0 (fromIntegral totalVertices)
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  GL.clientState GL.VertexArray $= Disabled
  GL.clientState GL.NormalArray $= Disabled
  GL.clientState GL.ColorArray $= Disabled

drawPlayers :: [Player] → Gui ()
drawPlayers p = do
  Static{scheme=Scheme{..}, guiConfig=GuiConfig{..}, bodyQuadric} ← ask
  lift $ do
  GL.materialAmbient Front $= ball_material_ambient
  GL.materialDiffuse Front $= ball_material_diffuse
  forM p $ \Player{..} → GL.preservingMatrix $ do
    GL.translate $ rayOrigin body
    GLU.gluSphere bodyQuadric playerSize 20 20
  return ()

drawFutures :: [Life] → IO ()
drawFutures ls = do
  GL.color green
  forM_ ls $ GL.renderPrimitive LineStrip . mapM_ (vertex . tov) . take 500 . positions

gui :: Controller c => SV.Vector StoredVertex → ObstacleTree → GuiConfig → (Gun → GunConfig) → GLdouble →
  c → IO c
gui storedObstacles tree guiConfig@GuiConfig{..} gunConfig initialCamYrot initialController = do

  let initialOrientation = CameraOrientation (zoom camConf 0) 0 initialCamYrot

  deepseq tree $ do

  scheme@Scheme{..} :: Scheme
    ← getDataFileName "schemes" >>= loadConfig . (++ "/" ++ schemeFile)

  let
    obstacleCount = SV.length storedObstacles `div` verticesPerObstacle
    initialState = State
      { controller = initialController
      , paused = True
      , camera = initialOrientation
      , guns = initialGuns }

  GLFW.initialize -- todo: check return value

  GLFW.openWindow GLFW.defaultDisplayOptions -- todo: check return value
    { GLFW.displayOptions_numRedBits = 8
    , GLFW.displayOptions_numGreenBits = 8
    , GLFW.displayOptions_numBlueBits = 8
    , GLFW.displayOptions_numDepthBits = 1
    }

  GL.depthFunc $= Just GL.Lequal
  GL.clearColor $= fog_color
  GL.lineWidth $= 3 -- Todo: Make configurable.
  GL.pointSize $= 4 -- Todo: Make configurable.
  GL.cullFace $= Just Back

  if ugly
   then do
    GL.lineSmooth $= Disabled
    GL.pointSmooth $= Disabled
    GL.normalize $= Disabled
    GL.shadeModel $= GL.Flat
    hint GL.LineSmooth $= GL.Fastest
    hint GL.PointSmooth $= GL.Fastest
   else do
    GL.lineSmooth $= Enabled
    GL.pointSmooth $= Enabled
    GL.shadeModel $= GL.Smooth
    hint GL.LineSmooth $= GL.Nicest
    hint GL.PointSmooth $= GL.Nicest

  GL.fog $= Enabled
  GL.fogMode $= GL.Exp2 fog_density
  GL.fogColor $= fog_color
  GL.lightModelAmbient $= lightModel_ambient
  GL.light ballLight $= Enabled
  GL.ambient ballLight $= ballLight_ambient
  GL.diffuse ballLight $= ballLight_diffuse
  GL.attenuation ballLight $= ballLight_attenuation
  GL.colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)

  GL.blend $= Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

  [obstacleBuffer] ← GL.genObjectNames 1
  let size = fromIntegral obstacleCount * bytesPerObstacle
  GL.bindBuffer GL.ArrayBuffer $= Just obstacleBuffer
  GL.bufferData GL.ArrayBuffer $= (size, SVP.ptr (SVP.cons storedObstacles), GL.StaticDraw)

  bodyQuadric ← GLU.gluNewQuadric

  r ← controller . runReaderT (glfwLoop initialState) Static{..}
  GLFW.closeWindow
  GLFW.terminate
  return r


f :: Controller c => Static → CameraOrientation → V → Gun → ClientGunState → CMS.State c ClientGunState
  -- todo: rename
f Static{..} o playerPos g ClientGunState{..} = do
    c <- CMS.get
    newFireState <- case (newTarget, fireState) of
      (Just t, FireAsap) | Just c' <- fire g (Just t) c → CMS.put c' >> return Fired
      (_, ReleaseAsap) | Just c' <- fire g Nothing c → CMS.put c' >> return Idle
      (_, s) -> return s
    return ClientGunState{target=newTarget, fireState=newFireState}
  where
    newTarget = findTarget tree playerPos (shootingRange (gunConfig g))
      $ gunRay o playerPos (gunGuiConfig guiConfig g)

gunDirection :: CameraOrientation -> GunGuiConfig -> V
gunDirection CameraOrientation{..} GunGuiConfig{..} = Vector3 0 0 (-1)
  `x_rot_vector` (gun_xrot + cam_xrot)
  `y_rot_vector` (gun_yrot + cam_yrot)

cameraOffset :: CameraOrientation → V
cameraOffset CameraOrientation{..} =
  Vector3 0 0 (- cam_dist) `x_rot_vector` cam_xrot `y_rot_vector` cam_yrot

gunRay :: CameraOrientation → V → GunGuiConfig → Ray
gunRay c playerPos g = Ray (playerPos <-> cameraOffset c) (gunDirection c g)

guiTick :: Controller c => State c → Gui (State c)
guiTick state@State{..} = do
  static@Static{..} <- ask

  if paused then return state else do

    -- errs ← get errors
    -- print $ "[" ++ (show errs) ++ "]"

  case player controller >>= birth of
    Nothing → return state
    Just Player{body} → do
      let (newGuns, newController) = CMS.runState (Map.traverseWithKey (f static camera (rayOrigin body)) guns) controller
      return state{controller=tick newController, guns=newGuns}

