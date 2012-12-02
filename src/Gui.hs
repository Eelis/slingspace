{-# LANGUAGE RecordWildCards, ViewPatterns, UnicodeSyntax, ScopedTypeVariables, StandaloneDeriving, PatternGuards, NamedFieldPuns, DeriveDataTypeable #-}

module Gui (Controller(..), Scheme(..), GuiConfig(..), GunGuiConfig(..), FloorConfig(..), GridType(..), CameraConfig(..), CameraOrientation(..), gui) where

import Data.Map (Map)
import Graphics.UI.GLUT (Vector3(..), GLdouble, ($=), Vertex3(..), Vertex4(..), Position(..), vertex, Flavour(..), MouseButton(..), PrimitiveMode(..), GLfloat, Color4(..), GLclampf, ClearBuffer(..), Face(..), KeyState(..), Capability(..), Key(..), hint, renderPrimitive, swapBuffers, lighting, ColorMaterialParameter(AmbientAndDiffuse), MatrixComponent, rotate)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Math (V, (<+>), (<->), (<*>), x_rot_vector, y_rot_vector, tov, wrap, normalize_v, Ray(..), Cube(..), trianglesPerObstacle, verticesPerTriangle, StoredVertex, bytesPerObstacle, verticesPerObstacle)
import Data.Maybe (isJust)
import Control.Monad (when, forM_)
import Data.Traversable (forM)
import Control.Monad.Fix (fix)
import Logic (Player(Player,body), Gun(..), Rope(..), findTarget, Life(..), positions, birth, GunConfig(shootingRange))
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Util ((.), getDataFileName, getMonotonicMilliSecs, whenJust, loadConfig)
import Prelude hiding ((.), mapM)
import Control.Monad.Reader (ReaderT(..), ask, asks, lift)
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.Storable (sizeOf)
import Data.Traversable (mapM)
import Control.DeepSeq (deepseq)
import Obstacles (ObstacleTree)
import Data.Typeable (Typeable)

import qualified Octree
import qualified Logic
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Pointer as SVP
import qualified Data.Map as Map
import qualified Graphics.UI.GLUT as GLUT
import qualified Control.Monad.State as CMS
import qualified Graphics.Rendering.OpenGL.GL as GL

deriving instance Read Key
deriving instance Read KeyState
deriving instance Read MouseButton
deriving instance Read GLUT.SpecialKey

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
  , zoomIn, zoomOut :: GLdouble -> GLdouble
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
  , restart_key, pause_key, exit_key, zoom_in_key, zoom_out_key :: (Key, KeyState)
  , tickDuration :: Integer -- in milliseconds
  } deriving Typeable

data Static = Static
  { obstacleBuffer :: GL.BufferObject
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
type Players = Map String Life

data State = State
  { controller :: Controller
  , paused :: Bool
  , camera :: CameraOrientation
  , guns :: Guns }

data Controller = Controller
  { players :: Players
  , tick :: Controller
  , release :: Gun → Maybe Controller
  , fire :: Gun → V → Maybe Controller }
  -- release and fire return Maybes so that the controller can say "nope, you can't release/fire now"




rotateRadians :: (Floating c, MatrixComponent c) ⇒ c → Vector3 c → IO ()
rotateRadians r = rotate (r / pi * 180)

green :: Color4 GLclampf
green = Color4 0 1 0 1

initialGuns :: Guns
initialGuns = Map.fromList $ flip (,) (ClientGunState Nothing Idle) . [LeftGun, RightGun]

onDisplay :: State → String → Gui ()
onDisplay State{controller=Controller{..}, camera=CameraOrientation{..}, ..} myname = do
  lift $ do
    GL.clear [ColorBuffer, DepthBuffer]
    GL.loadIdentity
    GL.translate $ Vector3 0 0 (- cam_dist)
    lighting $= Enabled
    GL.position ballLight $= Vertex4 30 30 100 1
    rotateRadians cam_xrot $ Vector3 1 0 0
    rotateRadians cam_yrot $ Vector3 0 1 0

  whenJust (Map.lookup myname players >>= birth) $ \me → do
  lift $ GL.translate $ (rayOrigin $ body me) <*> (-1)
  drawPlayers (Map.mapMaybe birth players)
  drawObstacles
  lift $ lighting $= Disabled
  --lift $ drawFutures players
  --drawTree tree
  drawFloor {-(shootableObstacles >>= obstacleTriangles)-} me
  drawRopes (Map.mapMaybe birth players)
  --drawOrientation (head . players)
  --drawSectorBorders $ head $ head $ Map.elems players
  drawCrossHairs guns
  lift swapBuffers

onReshape :: CameraConfig → GL.Size → IO ()
onReshape CameraConfig{..} s@(GL.Size w h) = do
  GL.viewport $= (Position 0 0, s)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GLUT.perspective fov (fromIntegral w / fromIntegral h) 10 viewing_dist
  GL.matrixMode $= GL.Modelview 0

gunForButton :: MouseButton -> Maybe Gun
gunForButton LeftButton = Just LeftGun
gunForButton RightButton = Just RightGun
gunForButton _ = Nothing

fireStateForKeyState :: KeyState → FireState
fireStateForKeyState Down = FireAsap
fireStateForKeyState Up = ReleaseAsap

onInput :: GuiConfig → Key → KeyState → State → Maybe State
onInput
    GuiConfig{camConf=CameraConfig{..}, ..}
    b bs state@State{controller=Controller{..}, ..} = do
  case () of
    _| k == pause_key → Just state{paused=not paused}
    _| k == exit_key → Nothing
    _| k == zoom_in_key →
      Just state{camera=camera{ cam_dist = zoomIn $ cam_dist camera }}
    _| k == zoom_out_key →
      Just state{camera=camera{ cam_dist = zoomOut $ cam_dist camera }}
    _| MouseButton (gunForButton → Just g) ← b → do
      Just state{guns=Map.adjust (\gs → gs { fireState = fireStateForKeyState bs }) g guns}
    _ → Just state
 where k = (b, bs)

onMotion :: CameraConfig → CameraOrientation → IORef Position → Position → IO CameraOrientation
onMotion CameraConfig{..} c@CameraOrientation{..} cursorPosRef p@(Position x y) = do
  Position x' y' ← readIORef cursorPosRef
  let q = Position (wrap x 100 400) (wrap y 100 400) -- Todo: This is no good.
  writeIORef cursorPosRef q
  when (q /= p) $ GLUT.pointerPosition $= q
  return $ if abs (x - x') + abs (y - y') < 200
    then c {
      cam_xrot = cam_xrot + (if invert_mouse then negate else id) (fromIntegral (y' - y) / mouse_speed),
      cam_yrot = cam_yrot + fromIntegral (x - x') / mouse_speed }
    else c

setupCallbacks :: State → String → Gui ()
setupCallbacks initialState name = do
  context@Static
    {guiConfig=guiConfig@GuiConfig{camConf=camConf@CameraConfig{..}, ..}, ..} ← ask
  lift $ do

  cursorPos ← newIORef $ Position 0 0
  stateRef ← newIORef initialState

  --lastDisplayTime ← getMonotonicMilliSecs >>= newIORef

  GLUT.reshapeCallback $= Just (onReshape camConf)

  GLUT.displayCallback $= do
    state ← readIORef stateRef
    runReaderT (onDisplay state name) context

    --new ← getMonotonicMilliSecs
    --old ← readIORef lastDisplayTime
    --print $ round $ (1000 :: Double) / fromIntegral (new - old) -- ++ " - " ++ replicate (fromIntegral $ new - old) 'x'
    --writeIORef lastDisplayTime new
  GLUT.keyboardMouseCallback $= Just (\x y _ _ → do
    s <- readIORef stateRef
    newState <- maybe (exitWith ExitSuccess) return (onInput guiConfig x y s)
    writeIORef stateRef newState
    when (paused s /= paused newState) $ do
      let
        (cursor, mc) = if paused newState then (GLUT.Inherit, Nothing) else (GLUT.None, Just $ \qq → do
          state <- readIORef stateRef
          newCam <- onMotion camConf (camera state) cursorPos qq
          writeIORef stateRef $ state{camera=newCam})
      GLUT.cursor $= cursor
      GLUT.motionCallback $= mc
      GLUT.passiveMotionCallback $= mc)

  (getMonotonicMilliSecs >>=) $ fix $ \self next → do
    state <- readIORef stateRef
    newState ← runReaderT (guiTick name state) context
    writeIORef stateRef newState
    tn ← getMonotonicMilliSecs
    let next' = next + tickDuration
    if tn >= next
      then self next'
      else GLUT.addTimerCallback (fromInteger $ next - tn) (self next')

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

drawRopes :: Map String Player → Gui ()
drawRopes players = do
  Scheme{..} ← asks scheme
  GuiConfig{..} ← asks guiConfig
  lift $ do
  GL.lineWidth $= rope_line_width
  renderPrimitive Lines $ forM players $ \player@Player{..} →
    forM_ (Map.toList (Logic.guns player)) $ \(gun, Rope{..}) → do
        GL.color $ gunColor gun
        vertex $ tov $ rayOrigin body <+> (normalize_v (rayOrigin rope_ray <-> rayOrigin body) <*> (playerSize + 0.05))
        vertex $ tov $ rayOrigin rope_ray
  return ()

drawOrientation :: Map String Player → Gui ()
drawOrientation players = do
  Scheme{..} ← asks scheme
  GuiConfig{..} ← asks guiConfig
  lift $ do
  GL.lineWidth $= rope_line_width
  renderPrimitive Lines $ forM players $ \Player{..} → do
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

drawPlayers :: Map String Player → Gui ()
drawPlayers players = do
  Scheme{..} ← asks scheme
  GuiConfig{..} ← asks guiConfig
  lift $ do
  GL.materialAmbient Front $= ball_material_ambient
  GL.materialDiffuse Front $= ball_material_diffuse
  forM players $ \Player{..} → GL.preservingMatrix $ do
    --print (rayOrigin body)
    GL.translate $ rayOrigin body
    GLUT.renderObject Solid $ GLUT.Sphere' playerSize 20 20
  return ()

drawFutures :: Players → IO ()
drawFutures players = do
  GL.color green
  forM_ (Map.elems players) $ GL.renderPrimitive LineStrip . mapM_ (vertex . tov) . take 500 . positions

-- Entry point:

gui :: Controller → SV.Vector StoredVertex → ObstacleTree → String → GuiConfig → (Gun -> GunConfig) → CameraOrientation → IO ()
gui controller storedObstacles tree name guiConfig@GuiConfig{..} gunConfig initialOrientation = do

  deepseq tree $ do

  scheme@Scheme{..} :: Scheme
    ← getDataFileName "schemes" >>= loadConfig . (++ "/" ++ schemeFile)

  let
    obstacleCount = SV.length storedObstacles `div` verticesPerObstacle
    initialState = State
      { controller = controller
      , paused = True
      , camera = initialOrientation
      , guns = initialGuns }

  GLUT.getArgsAndInitialize

  GLUT.initialDisplayMode $= [GLUT.DoubleBuffered, GLUT.WithDepthBuffer, GLUT.RGBMode]
  GLUT.createWindow windowTitle
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

  runReaderT (setupCallbacks initialState name) Static{..}
  GLUT.mainLoop

f :: Static -> CameraOrientation -> V -> Gun -> ClientGunState -> CMS.State Controller ClientGunState
  -- todo: rename
f Static{..} o playerPos g ClientGunState{..} = do
    c <- CMS.get
    newFireState <- case (newTarget, fireState) of
      (Just t, FireAsap) | Just c' <- fire c g t → CMS.put c' >> return Fired
      (_, ReleaseAsap) | Just c' <- release c g → CMS.put c' >> return Idle
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

guiTick :: String → State → Gui State
guiTick myname state@State{..} = do
  static@Static{..} <- ask

  lift $ GLUT.postRedisplay Nothing

  if paused then return state else do

    -- errs ← get errors
    -- print $ "[" ++ (show errs) ++ "]"
  
  case Map.lookup myname (players controller) >>= birth of
    Nothing → return state
    Just Player{body} → do
      let (newGuns, newController) = CMS.runState (Map.traverseWithKey (f static camera (rayOrigin body)) guns) controller
      return state{controller=tick newController, guns=newGuns}
