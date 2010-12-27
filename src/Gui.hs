{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Gui (Controller(..), State(..), gui) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (Vector3(..), GLdouble, ($=), Vertex3(..), Vertex4(..), Position(..), vertex, Flavour(..), MouseButton(..), PrimitiveMode(..), GLfloat, Color4(..), GLclampf, ClearBuffer(..), Face(..), KeyState(..), Capability(..), Key(..), hint, renderPrimitive, swapBuffers, lighting)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef, writeIORef)
import Math (V, (<+>), (<->), (<*>), x_rot_vector, y_rot_vector, tov, wrap, AnnotatedTriangle(..), normalize_v, vectorToNormal, Ray(..), obstacleTriangles, GeometricObstacle, VisualObstacle(..))
import Maybe (isJust)
import Control.Monad (when, forM_)
import Data.Traversable (forM)
import Control.Monad.Fix (fix)
import Control.Monad (foldM)
import Logic (Player(..), Gun(..), GameplayConfig(..), Rope(..), find_target, toFloor)
import MyGL (rotateRadians, green)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import MyUtil ((.), getDataFileName, read_config_file, getMonotonicMilliSecs, tupleToList, whenJust)
import Prelude hiding ((.))
import Control.Monad.Reader (ReaderT(..), ask, asks, lift)
import Foreign.Ptr (nullPtr, plusPtr)
import TerrainGenerator (bytesPerVertex, totalVertices, totalBytes, bytesPerDouble, doublesPerVector)

type SectorId = Vector3 Integer

-- Configuration:

data Scheme = Scheme
  { shadow_color :: Color4 GLclampf
  , grid_color :: Color4 GLclampf
  , rope_line_width :: GLfloat
  , fog_density :: GLfloat
  , fog_color, left_gun_color, right_gun_color :: Color4 GLclampf
  , lightModel_ambient
  , ballLight_ambient, ballLight_diffuse
  , ball_material_ambient, ball_material_diffuse :: Color4 GLfloat
  , ballLight_attenuation :: (GLfloat, GLfloat, GLfloat)
  } deriving Read

gunColor :: Scheme → Gun → Color4 GLclampf
  -- We don't use this function type in Scheme itself because then we can't derive Read.
gunColor Scheme{..} LeftGun = left_gun_color
gunColor Scheme{..} RightGun = right_gun_color

data GridType =
  DottedGrid { grid_dot_size :: GLfloat } |
  LinedGrid { grid_line_width :: GLfloat }
    deriving Read

data FloorConfig = Grid { grid_size :: Integer, grid_type :: GridType } | Shadows deriving Read

data CameraConfig = CameraConfig
  { viewing_dist :: GLdouble
  , fov :: GLdouble -- in degrees
  , cam_init_dist, cam_min_dist, cam_max_dist :: GLdouble
  , cam_zoom_speed :: GLdouble -- in camera distance multiplication per increment
  , mouse_speed :: GLdouble -- in pixels per radian
  , invert_mouse :: Bool
  } deriving Read

data GunGuiConfig = GunGuiConfig { gun_xrot, gun_yrot :: GLdouble }

data GuiConfig = GuiConfig
  { windowTitle :: String
  , cross_offset_hor, cross_offset_ver :: GLdouble -- in radians
  , ugly :: Bool
  , floorConf :: Maybe FloorConfig
  , playerSize :: GLdouble
  , camConf :: CameraConfig
  , schemeFile :: String
  , restart_key, pause_key, exit_key, zoom_in_key, zoom_out_key :: (Key, KeyState)
  } deriving Read

gunGuiConfig :: GuiConfig → Gun → GunGuiConfig
gunGuiConfig cf LeftGun = GunGuiConfig (- cross_offset_ver cf) (- cross_offset_hor cf)
gunGuiConfig cf RightGun = GunGuiConfig (- cross_offset_ver cf) (cross_offset_hor cf)

tickDurationMilliSecs :: Integer
tickDurationMilliSecs = 10  -- Todo: Make configurable.

-- State:

data Camera = Camera { cam_dist, cam_xrot, cam_yrot :: !GLdouble }
data FireState = FireAsap | Fired | Idle
data ClientGunState = ClientGunState { target :: Maybe V, fire_state :: FireState }
type ClientState = Map Gun ClientGunState

data State = State
  { players :: Map String [Player]
  , shootableObstacles :: [GeometricObstacle] }

data Controller = Controller
  { state :: State
  , tick :: GLUT.BufferObject → IO Controller
  , release :: Gun → IO Controller
  , fire :: Gun → V → IO Controller
  , spawn :: IO Controller }

initialClientState :: ClientState
initialClientState = Map.fromList $ flip (,) (ClientGunState Nothing Idle) . [LeftGun, RightGun]

-- Callbacks:

data GuiContext = GuiContext { obstacleBuffer :: GLUT.BufferObject, scheme :: Scheme, guiConfig :: GuiConfig }
type Gui = ReaderT GuiContext IO

onDisplay :: State → String → Camera → ClientState → Gui ()
onDisplay State{..} myname Camera{..} clientState = do
  lift $ do
    GLUT.clear [ColorBuffer, DepthBuffer]
    GLUT.loadIdentity
    GLUT.translate $ Vector3 0 0 (- cam_dist)
    lighting $= Enabled
    GLUT.position ballLight $= Vertex4 30 30 100 1
    rotateRadians cam_xrot $ Vector3 1 0 0
    rotateRadians cam_yrot $ Vector3 0 1 0
  whenJust (Map.lookup myname players) $ \(me:_) → do
  lift $ GLUT.translate $ (rayOrigin $ body me) <*> (-1)
  drawPlayers (head . players)
  drawObstacles
  lift $ lighting $= Disabled
  -- lift $ drawFutures players
  --drawFloor (shootableObstacles >>= obstacleTriangles) me
  drawRopes (head . players)
  drawCrossHairs clientState
  lift swapBuffers

onReshape :: CameraConfig → GLUT.Size → IO ()
onReshape CameraConfig{..} s@(GLUT.Size w h) = do
  GLUT.viewport $= (Position 0 0, s)
  GLUT.matrixMode $= GLUT.Projection
  GLUT.loadIdentity
  GLUT.perspective fov (fromIntegral w / fromIntegral h) 0.1 viewing_dist
  GLUT.matrixMode $= GLUT.Modelview 0

onInput :: GuiConfig → IORef ClientState → IORef Bool → IORef Camera → IORef Position → Key → KeyState → a → b → Controller → IO Controller
onInput GuiConfig{camConf=camConf@CameraConfig{..}, ..} clientStateRef pauseRef cameraRef cursorPos b bs _ _ controller@Controller{..} =
  case () of
    _| k == restart_key → spawn
    _| k == pause_key → togglePause camConf pauseRef cameraRef cursorPos >> return controller
    _| k == exit_key → exitWith ExitSuccess
    _| k == zoom_in_key → do
      modifyIORef cameraRef $ \cam → cam { cam_dist = max (cam_dist cam / cam_zoom_speed) cam_min_dist }
      return controller
    _| k == zoom_out_key → do
      modifyIORef cameraRef $ \cam → cam { cam_dist = min (cam_dist cam * cam_zoom_speed) cam_max_dist }
      return controller
    _| otherwise → case (b, bs) of
     (MouseButton LeftButton, Down) → do fire_asap_a LeftGun; return controller
     (MouseButton LeftButton, Up) → release_a LeftGun
     (MouseButton MiddleButton, Down) → do fire_asap_a RightGun; return controller
     (MouseButton MiddleButton, Up) → release_a RightGun
     (MouseButton RightButton, Down) → do fire_asap_a RightGun; return controller
     (MouseButton RightButton, Up) → release_a RightGun
     _ → return controller
 where
  k = (b, bs)
  fire_asap_a = modifyIORef clientStateRef . Map.adjust (\g → g { fire_state = FireAsap })
  release_a u = do
    modifyIORef clientStateRef (Map.adjust (\g → g { fire_state = Idle }) u)
    release u

onMotion :: CameraConfig → IORef Camera → IORef Position → Position → IO ()
onMotion CameraConfig{..} cameraRef cursorPosRef p@(Position x y) = do
  Position x' y' ← readIORef cursorPosRef
  let q = Position (wrap x 100 400) (wrap y 100 400) -- Todo: This is no good.
  writeIORef cursorPosRef q
  when (q /= p) $ GLUT.pointerPosition $= q
  when (abs (x - x') + abs (y - y') < 200) $ do
  modifyIORef cameraRef (\c@Camera{..} → c {
    cam_xrot = cam_xrot + (if invert_mouse then negate else id) (fromIntegral (y' - y) / mouse_speed),
    cam_yrot = cam_yrot + fromIntegral (x - x') / mouse_speed })
  GLUT.postRedisplay Nothing

setupCallbacks :: Controller → IORef ClientState → String → GameplayConfig → Gui ()
setupCallbacks initialController clientStateRef name gameplayConfig = do
  context@GuiContext
    {guiConfig=guiConfig@GuiConfig{camConf=camConf@CameraConfig{..}, ..}, ..} ← ask
  lift $ do

  pauseRef ← newIORef True
  cursorPos ← newIORef $ Position 0 0
  cameraRef ← newIORef $ Camera cam_init_dist 0 0
  controllerRef ← newIORef initialController

  lastDisplayTime ← getMonotonicMilliSecs >>= newIORef

  GLUT.reshapeCallback $= Just (onReshape camConf)
  GLUT.displayCallback $= do
    camera ← readIORef cameraRef
    clientState ← readIORef clientStateRef
    Controller{..} ← readIORef controllerRef
    runReaderT (onDisplay state name camera clientState) context
    new ← getMonotonicMilliSecs
    old ← readIORef lastDisplayTime
    putStrLn $ replicate (fromIntegral $ new - old) 'x'
    writeIORef lastDisplayTime new
  GLUT.keyboardMouseCallback $= Just (\ x y z w →
    readIORef controllerRef
      >>= onInput guiConfig clientStateRef pauseRef cameraRef cursorPos x y z w
      >>= writeIORef controllerRef)

  (getMonotonicMilliSecs >>=) $ fix $ \self next → do
    controller ← readIORef controllerRef >>=
      guiTick context pauseRef cameraRef clientStateRef name gameplayConfig
    writeIORef controllerRef controller
    tn ← getMonotonicMilliSecs
    let next' = next + tickDurationMilliSecs
    if tn >= next
      then self next'
      else GLUT.addTimerCallback (fromInteger $ next - tn) (self next')

-- Drawers:

ballLight :: GLUT.Light
ballLight = GLUT.Light 0

drawFloor :: [AnnotatedTriangle] → Player → Gui ()
drawFloor visible_obs Player{..} = do
  Scheme{..} ← asks scheme
  GuiConfig{camConf=CameraConfig{..}, ..} ← asks guiConfig
  lift $ do
  whenJust floorConf $ \kind → do
  case kind of
    Shadows → do
      GLUT.color shadow_color
      GLUT.renderPrimitive Triangles $ forM_ visible_obs $
        mapM (vertex . tov . toFloor) . tupleToList . triangleVertices
    Grid{..} → do
      GLUT.color grid_color
      let
        Vector3 x _ z = rayOrigin body
        funky h = fromInteger (h' - (h' `mod` grid_size)) :: GLdouble where h' = round h :: Integer
        aligned_z = funky z; aligned_x = funky x
        vd = viewing_dist
      case grid_type of
        LinedGrid{..} → do
          GLUT.lineWidth $= grid_line_width
          renderPrimitive Lines $
            forM_ [-vd, -vd + (fromInteger grid_size) .. vd] $ \n →
              mapM (vertex . tov) $
                [ Vector3 (aligned_x + n) 0 (z - vd), Vector3 (aligned_x + n) 0 (z + vd)
                , Vector3 (x - vd) 0 (aligned_z + n), Vector3 (x + vd) 0 (aligned_z + n) ]
        DottedGrid{..} → do
          GLUT.pointSize $= grid_dot_size
          GLUT.renderPrimitive Points $
            forM_ [(aligned_x + x', aligned_z + z') | x' ← [-vd, -vd + (fromInteger grid_size) .. vd], z' ← [-vd, -vd + (fromInteger grid_size) .. vd]] $ \(x', z') →
              vertex $ tov $ Vector3 x' 0 z'

drawCrossHairs :: ClientState → Gui ()
drawCrossHairs clientState = do
  scheme ← asks scheme
  guiConfig ← asks guiConfig
  lift $ do
  GLUT.lineWidth $= 3
  GLUT.pointSize $= 4
  forM_ (Map.toList clientState) $ \(g, gu) → do
    let GunGuiConfig{..} = gunGuiConfig guiConfig g
    GLUT.color $ gunColor scheme g
    GLUT.loadIdentity
    rotateRadians gun_xrot $ Vector3 (-1) 0 0
    rotateRadians gun_yrot $ Vector3 0 (-1) 0
    GLUT.renderPrimitive Points $ vertex $ Vertex3 (0 :: GLdouble) 0 (-100)
    when (isJust $ target gu) $ GLUT.renderPrimitive LineLoop $ mapM_ vertex
      [ Vertex3 (-1 :: GLdouble) 0 (-100)
      , Vertex3 (0 :: GLdouble) (-1) (-100)
      , Vertex3 (1 :: GLdouble) 0 (-100)
      , Vertex3 (0 :: GLdouble) 1 (-100) ]

drawRopes :: Map String Player → Gui ()
drawRopes players = do
  scheme@Scheme{..} ← asks scheme
  GuiConfig{..} ← asks guiConfig
  lift $ do
  GLUT.lineWidth $= rope_line_width
  renderPrimitive Lines $ forM players $ \Player{..} →
    forM_ (Map.toList guns) $ \(gun, Rope{..}) → do
        GLUT.color $ gunColor scheme gun
        vertex $ tov $ rayOrigin body <+> (normalize_v (rayOrigin rope_ray <-> rayOrigin body) <*> (playerSize + 0.05))
        vertex $ tov $ rayOrigin rope_ray
  return ()

drawObstacles :: Gui ()
drawObstacles = do
  GuiContext{scheme=Scheme{..}, ..} ← ask
  lift $ do
  GLUT.materialDiffuse Front $= Color4 1 1 1 1
  GLUT.materialAmbient Front $= Color4 0.4 0.6 0.8 1
  GLUT.clientState GLUT.VertexArray $= Enabled
  GLUT.clientState GLUT.NormalArray $= Enabled
  GLUT.bindBuffer GLUT.ArrayBuffer $= Just obstacleBuffer
  GLUT.arrayPointer GLUT.VertexArray $= GLUT.VertexArrayDescriptor 3 GLUT.Double bytesPerVertex nullPtr
  GLUT.arrayPointer GLUT.NormalArray $= GLUT.VertexArrayDescriptor 3 GLUT.Double bytesPerVertex (plusPtr nullPtr (doublesPerVector*bytesPerDouble))
  GLUT.drawArrays Triangles 0 (fromIntegral totalVertices)
  GLUT.bindBuffer GLUT.ArrayBuffer $= Nothing
  GLUT.clientState GLUT.VertexArray $= Disabled
  GLUT.clientState GLUT.NormalArray $= Disabled

drawPlayers :: Map String Player → Gui ()
drawPlayers players = do
  Scheme{..} ← asks scheme
  GuiConfig{..} ← asks guiConfig
  lift $ do
  GLUT.materialAmbient Front $= ball_material_ambient
  GLUT.materialDiffuse Front $= ball_material_diffuse
  forM players $ \Player{..} → GLUT.preservingMatrix $ do
    GLUT.translate $ rayOrigin body
    GLUT.renderObject Solid $ GLUT.Sphere' playerSize 20 20
  return ()

drawFutures :: Map String [Player] → IO ()
drawFutures players = do
  GLUT.color green
  forM players $ GLUT.renderPrimitive LineStrip . mapM_ (vertex . tov . rayOrigin . body) . take 500
  return ()

-- Entry point:

gui :: Controller → String → GameplayConfig → IO ()
gui controller name gameplayConfig = do

  guiConfig@GuiConfig{..} :: GuiConfig
    ← read_config_file "gui.txt"
  scheme@Scheme{..} :: Scheme
    ← read . (readFile =<< (++ "/" ++ schemeFile) . getDataFileName "schemes")
  clientState ← newIORef initialClientState

  GLUT.getArgsAndInitialize

  GLUT.initialDisplayMode $= [GLUT.DoubleBuffered, GLUT.WithDepthBuffer, GLUT.WithAlphaComponent, GLUT.RGBAMode]
  GLUT.createWindow windowTitle
  GLUT.depthFunc $= Just GLUT.Lequal
  GLUT.clearColor $= fog_color
  GLUT.lineWidth $= 3 -- Todo: Make configurable.
  GLUT.pointSize $= 4 -- Todo: Make configurable.
  GLUT.cullFace $= Just Back

  if ugly
   then do
    GLUT.lineSmooth $= Disabled
    GLUT.pointSmooth $= Disabled
    GLUT.normalize $= Disabled
    GLUT.shadeModel $= GLUT.Flat
    hint GLUT.LineSmooth $= GLUT.Fastest
    hint GLUT.PointSmooth $= GLUT.Fastest
   else do
    GLUT.lineSmooth $= Enabled
    GLUT.pointSmooth $= Enabled
    GLUT.shadeModel $= GLUT.Smooth
    hint GLUT.LineSmooth $= GLUT.Nicest
    hint GLUT.PointSmooth $= GLUT.Nicest

  GLUT.fog $= Enabled
  GLUT.fogMode $= GLUT.Exp2 fog_density
  GLUT.fogColor $= fog_color
  GLUT.lightModelAmbient $= lightModel_ambient
  GLUT.light ballLight $= Enabled
  GLUT.ambient ballLight $= ballLight_ambient
  GLUT.diffuse ballLight $= ballLight_diffuse
  GLUT.attenuation ballLight $= ballLight_attenuation

  GLUT.blend $= Enabled
  GLUT.blendFunc $= (GLUT.SrcAlpha, GLUT.OneMinusSrcAlpha)

  [obstacleBuffer] ← GLUT.genObjectNames 1
  GLUT.bindBuffer GLUT.ArrayBuffer $= Just obstacleBuffer
  GLUT.bufferData GLUT.ArrayBuffer $= (fromIntegral totalBytes, nullPtr, GLUT.DynamicDraw)
  GLUT.bindBuffer GLUT.ArrayBuffer $= Nothing

  runReaderT (setupCallbacks controller clientState name gameplayConfig) GuiContext{..}

  GLUT.mainLoop

-- Logic:

togglePause :: CameraConfig → IORef Bool → IORef Camera → IORef Position → IO ()
togglePause cam_conf pauseRef cameraRef cursorPosRef = do
  p ← readIORef pauseRef
  writeIORef pauseRef $ not p
  let
    (c, mc) = if p then (GLUT.None, Just $ \qq → do
    onMotion cam_conf cameraRef cursorPosRef qq) else (GLUT.Inherit, Nothing)
  GLUT.cursor $= c
  GLUT.motionCallback $= mc
  GLUT.passiveMotionCallback $= mc

guiTick :: GuiContext → IORef Bool → IORef Camera → IORef ClientState → String → GameplayConfig → Controller → IO Controller
guiTick GuiContext{..} pauseRef cameraRef clientStateRef myname gameplayConfig controller = do
  paused ← readIORef pauseRef
  if paused then return controller else do

    -- errs ← get errors
    -- print $ "[" ++ (show errs) ++ "]"

  let maybePlayer = Map.lookup myname $ players $ state controller
  case maybePlayer of
    Nothing → return controller
    Just (player@Player{..}:_) → do
      Camera{..} ← readIORef cameraRef

      let cam_pos = rayOrigin body <-> (Vector3 0 0 (- cam_dist) `x_rot_vector` cam_xrot `y_rot_vector` cam_yrot)

      clientState ← readIORef clientStateRef

      writeIORef clientStateRef $ flip Map.mapWithKey clientState $
        \(gunGuiConfig guiConfig → GunGuiConfig{..}) g → g { target =
        find_target (shootableObstacles $ state controller) player gameplayConfig $ Ray cam_pos $ Vector3 0 0 (-1)
          `x_rot_vector` (gun_xrot + cam_xrot)
          `y_rot_vector` (gun_yrot + cam_yrot) }

      newController ← foldM  (\gs (k, v) →
        case v of
          ClientGunState (Just t) FireAsap → do
            modifyIORef clientStateRef $ Map.adjust (\g → g { fire_state = Fired }) k
            fire gs k t
          _ → return gs)
          controller
          (Map.toList clientState)
      GLUT.postRedisplay Nothing
      tick newController obstacleBuffer
