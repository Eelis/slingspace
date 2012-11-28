{-# LANGUAGE RecordWildCards, ViewPatterns, UnicodeSyntax, ScopedTypeVariables, StandaloneDeriving, PatternGuards, NamedFieldPuns, DeriveDataTypeable #-}

module Gui (Controller(..), Scheme(..), GuiConfig(..), GunGuiConfig(..), FloorConfig(..), GridType(..), CameraConfig(..), gui) where

import Data.Map (Map)
import Graphics.UI.GLUT (Vector3(..), GLdouble, ($=), Vertex3(..), Vertex4(..), Position(..), vertex, Flavour(..), MouseButton(..), PrimitiveMode(..), GLfloat, Color4(..), GLclampf, ClearBuffer(..), Face(..), KeyState(..), Capability(..), Key(..), hint, renderPrimitive, swapBuffers, lighting, ColorMaterialParameter(AmbientAndDiffuse))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Math (V, (<+>), (<->), (<*>), x_rot_vector, y_rot_vector, tov, wrap, normalize_v, Ray(..), Cube(..), trianglesPerObstacle, verticesPerTriangle, StoredVertex)
import Data.Maybe (isJust)
import Control.Monad (when, forM_)
import Data.Traversable (forM)
import Control.Monad.Fix (fix)
import Logic (Player(Player,body), Gun(..), Rope(..), findTarget, Life(..), moments, birth, GunConfig(shootingRange))
import MyGL (rotateRadians, green)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import MyUtil ((.), getDataFileName, getMonotonicMilliSecs, whenJust, loadConfig)
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
import qualified TerrainGenerator
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Pointer as SVP
import qualified Data.Map as Map
import qualified Graphics.UI.GLUT as GLUT
import qualified Control.Monad.State as CMS

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
  , cam_init_dist, cam_min_dist, cam_max_dist :: GLdouble
  , cam_zoom_speed :: GLdouble -- in camera distance multiplication per increment
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
  } deriving Typeable

data Static = Static
  { obstacleBuffer :: GLUT.BufferObject
  , scheme :: Scheme
  , guiConfig :: GuiConfig
  , gunConfig :: Gun -> GunConfig -- not part of GuiConfig because gunConfig is normally read from a gameplay config file
  }

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
  , guns :: Guns
  , obstacleCount :: Int
  , tree :: ObstacleTree }

type ObstacleUpdate = (SV.Vector StoredVertex, ObstacleTree)

data Controller = Controller
  { players :: Players
  , tick :: IO (Maybe ObstacleUpdate, Controller)
  , release :: Gun → Maybe Controller
  , fire :: Gun → V → Maybe Controller }
  -- release and fire return Maybes so that the controller can say "nope, you can't release/fire now"





tickDurationMilliSecs :: Integer
tickDurationMilliSecs = 10  -- Todo: Make configurable.

initialGuns :: Guns
initialGuns = Map.fromList $ flip (,) (ClientGunState Nothing Idle) . [LeftGun, RightGun]

onDisplay :: State → String → Gui ()
onDisplay State{controller=Controller{..}, camera=CameraOrientation{..}, ..} myname = do
  lift $ do
    GLUT.clear [ColorBuffer, DepthBuffer]
    GLUT.loadIdentity
    GLUT.translate $ Vector3 0 0 (- cam_dist)
    lighting $= Enabled
    GLUT.position ballLight $= Vertex4 30 30 100 1
    rotateRadians cam_xrot $ Vector3 1 0 0
    rotateRadians cam_yrot $ Vector3 0 1 0

  whenJust (Map.lookup myname players >>= birth) $ \me → do
  lift $ GLUT.translate $ (rayOrigin $ body me) <*> (-1)
  drawPlayers (Map.mapMaybe birth players)
  drawObstacles obstacleCount
  lift $ lighting $= Disabled
  --lift $ drawFutures players
  --drawTree tree
  drawFloor {-(shootableObstacles >>= obstacleTriangles)-} me
  drawRopes (Map.mapMaybe birth players)
  --drawOrientation (head . players)
  --drawSectorBorders $ head $ head $ Map.elems players
  drawCrossHairs guns
  lift swapBuffers

onReshape :: CameraConfig → GLUT.Size → IO ()
onReshape CameraConfig{..} s@(GLUT.Size w h) = do
  GLUT.viewport $= (Position 0 0, s)
  GLUT.matrixMode $= GLUT.Projection
  GLUT.loadIdentity
  GLUT.perspective fov (fromIntegral w / fromIntegral h) 0.1 viewing_dist
  GLUT.matrixMode $= GLUT.Modelview 0

gunForButton :: MouseButton -> Gun
gunForButton LeftButton = LeftGun
gunForButton _ = RightGun

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
      Just state{camera=camera{ cam_dist = max (cam_dist camera / cam_zoom_speed) cam_min_dist }}
    _| k == zoom_out_key →
      Just state{camera=camera{ cam_dist = min (cam_dist camera * cam_zoom_speed) cam_max_dist }}
    _| MouseButton button ← b → do
      Just state{guns=Map.adjust (\gs → gs { fireState = fireStateForKeyState bs }) (gunForButton button) guns}
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
    let next' = next + tickDurationMilliSecs
    if tn >= next
      then self next'
      else GLUT.addTimerCallback (fromInteger $ next - tn) (self next')

-- Drawers:

ballLight :: GLUT.Light
ballLight = GLUT.Light 0

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
  GLUT.lineWidth $= 3
  GLUT.pointSize $= 4
  forM_ (Map.toList guns) $ \(g, gu) → do
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
  Scheme{..} ← asks scheme
  GuiConfig{..} ← asks guiConfig
  lift $ do
  GLUT.lineWidth $= rope_line_width
  renderPrimitive Lines $ forM players $ \player@Player{..} →
    forM_ (Map.toList (Logic.guns player)) $ \(gun, Rope{..}) → do
        GLUT.color $ gunColor gun
        vertex $ tov $ rayOrigin body <+> (normalize_v (rayOrigin rope_ray <-> rayOrigin body) <*> (playerSize + 0.05))
        vertex $ tov $ rayOrigin rope_ray
  return ()

drawOrientation :: Map String Player → Gui ()
drawOrientation players = do
  Scheme{..} ← asks scheme
  GuiConfig{..} ← asks guiConfig
  lift $ do
  GLUT.lineWidth $= rope_line_width
  renderPrimitive Lines $ forM players $ \Player{..} → do
    vertex $ tov $ rayOrigin body <+> Vector3 (-100) 0 0
    vertex $ tov $ rayOrigin body <+> Vector3 100 0 0
    vertex $ tov $ rayOrigin body <+> Vector3 0 0 (-100)
    vertex $ tov $ rayOrigin body <+> Vector3 0 0 100
  return ()

drawObstacles :: Int → Gui ()
drawObstacles obstacleCount = do
  Static{scheme=Scheme{..}, ..} ← ask
  lift $ do
  
  GLUT.materialDiffuse Front $= Color4 1 1 1 1
  GLUT.materialAmbient Front $= Color4 0.4 0.6 0.8 1
  GLUT.clientState GLUT.VertexArray $= Enabled
  GLUT.clientState GLUT.NormalArray $= Enabled
  GLUT.clientState GLUT.ColorArray $= Enabled
  GLUT.bindBuffer GLUT.ArrayBuffer $= Just obstacleBuffer
  let bytesPerVertex = fromIntegral $ sizeOf (undefined :: StoredVertex)
  let bytesPerVector = fromIntegral $ sizeOf (undefined :: Vector3 GLdouble)
  GLUT.arrayPointer GLUT.VertexArray
    $= GLUT.VertexArrayDescriptor 3 GLUT.Double bytesPerVertex (plusPtr nullPtr (0 * bytesPerVector))
  GLUT.arrayPointer GLUT.NormalArray
    $= GLUT.VertexArrayDescriptor 3 GLUT.Double bytesPerVertex (plusPtr nullPtr (1 * bytesPerVector))
  GLUT.arrayPointer GLUT.ColorArray
    $= GLUT.VertexArrayDescriptor 3 GLUT.Double bytesPerVertex (plusPtr nullPtr (2 * bytesPerVector))

  let totalVertices = obstacleCount * trianglesPerObstacle * verticesPerTriangle
  GLUT.drawArrays Triangles 0 (fromIntegral totalVertices)
  GLUT.bindBuffer GLUT.ArrayBuffer $= Nothing
  GLUT.clientState GLUT.VertexArray $= Disabled
  GLUT.clientState GLUT.NormalArray $= Disabled
  GLUT.clientState GLUT.ColorArray $= Disabled

drawPlayers :: Map String Player → Gui ()
drawPlayers players = do
  Scheme{..} ← asks scheme
  GuiConfig{..} ← asks guiConfig
  lift $ do
  GLUT.materialAmbient Front $= ball_material_ambient
  GLUT.materialDiffuse Front $= ball_material_diffuse
  forM players $ \Player{..} → GLUT.preservingMatrix $ do
    --print (rayOrigin body)
    GLUT.translate $ rayOrigin body
    GLUT.renderObject Solid $ GLUT.Sphere' playerSize 20 20
  return ()

drawFutures :: Players → IO ()
drawFutures players = do
  GLUT.color green
  forM_ (Map.elems players) $ GLUT.renderPrimitive LineStrip . mapM_ (vertex . tov . rayOrigin . body) . take 500 . moments

-- Entry point:

gui :: Controller → ObstacleUpdate → String → GuiConfig → (Gun -> GunConfig) → IO ()
gui controller (storedObstacles, tree) name guiConfig@GuiConfig{..} gunConfig = do

  deepseq tree $ do

  scheme@Scheme{..} :: Scheme
    ← getDataFileName "schemes" >>= loadConfig . (++ "/" ++ schemeFile)

  let
    initialState = State
      { controller = controller
      , paused = True
      , camera = CameraOrientation (cam_init_dist camConf) 0 pi
      , guns = initialGuns
      , obstacleCount = SV.length storedObstacles `div` TerrainGenerator.verticesPerObstacle
      , tree = tree }

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
  GLUT.colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)

  GLUT.blend $= Enabled
  GLUT.blendFunc $= (GLUT.SrcAlpha, GLUT.OneMinusSrcAlpha)

  [obstacleBuffer] ← GLUT.genObjectNames 1
  let size = fromIntegral (obstacleCount initialState) * TerrainGenerator.bytesPerObstacle
  GLUT.bindBuffer GLUT.ArrayBuffer $= Just obstacleBuffer
  GLUT.bufferData GLUT.ArrayBuffer $= (size, nullPtr, GLUT.DynamicDraw)
  GLUT.bufferSubData GLUT.ArrayBuffer GLUT.WriteToBuffer 0 size (SVP.ptr (SVP.cons storedObstacles))
    -- todo: merge into one
  GLUT.bindBuffer GLUT.ArrayBuffer $= Nothing

  runReaderT (setupCallbacks initialState name) Static{..}
  GLUT.mainLoop

f :: Static -> CameraOrientation -> ObstacleTree -> V -> Gun -> ClientGunState -> CMS.State Controller ClientGunState
  -- todo: rename
f Static{..} o tree playerPos g ClientGunState{..} = do
    c <- CMS.get
    newFireState <- case (newTarget, fireState) of
      (Just t, FireAsap) | Just c' <- fire c g t → CMS.put c' >> return Fired
      (_, ReleaseAsap) | Just c' <- release c g → CMS.put c' >> return Idle
      (_, s) -> return s
    return ClientGunState{target=newTarget, fireState=newFireState}
  where
    newTarget = findTarget tree playerPos (shootingRange (gunConfig g))
      $ cameraRay o playerPos (gunGuiConfig guiConfig g)

cameraDirection :: CameraOrientation -> GunGuiConfig -> V
cameraDirection CameraOrientation{..} GunGuiConfig{..} = Vector3 0 0 (-1)
  `x_rot_vector` (gun_xrot + cam_xrot)
  `y_rot_vector` (gun_yrot + cam_yrot)

cameraRay :: CameraOrientation -> V -> GunGuiConfig -> Ray
cameraRay c@CameraOrientation{..} playerPos g = Ray
  (playerPos <-> (Vector3 0 0 (- cam_dist) `x_rot_vector` cam_xrot `y_rot_vector` cam_yrot))
  (cameraDirection c g)

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
      let
        (newGuns, newController) = CMS.runState (Map.traverseWithKey (f static camera tree (rayOrigin body)) guns) controller
      (mx, c) <- lift $ tick newController
      case mx of
        Nothing -> return state{controller=c, guns=newGuns}
        Just (a, newTree) -> do
          let newObstacleCount = SV.length a `div` TerrainGenerator.verticesPerObstacle
          lift $ GLUT.bindBuffer GLUT.ArrayBuffer $= Just obstacleBuffer
          lift $ GLUT.bufferSubData GLUT.ArrayBuffer GLUT.WriteToBuffer 0
              (fromIntegral newObstacleCount * TerrainGenerator.bytesPerObstacle) (SVP.ptr (SVP.cons a))
          return state{controller=c, obstacleCount=newObstacleCount, tree=newTree,guns=newGuns}
