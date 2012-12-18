{-# LANGUAGE RecordWildCards, UnicodeSyntax, ScopedTypeVariables, PatternGuards, NamedFieldPuns, DeriveDataTypeable, MultiWayIf, LambdaCase, Rank2Types, FlexibleContexts #-}

module Gui (Scheme(..), GuiConfig(..), GunGuiConfig(..), FloorConfig(..), GridType(..), CameraConfig(..), gui) where

import Prelude hiding ((.), mapM)

import Graphics.Rendering.OpenGL.GL
  (GLfloat, GLdouble, GLclampf, Vector3(..), Vertex3(..), Vertex4(..), Color4(..), ($=), Capability(Enabled, Disabled))
import Foreign.Ptr (nullPtr, plusPtr, Ptr)
import Foreign.Storable (sizeOf)
import Data.AdditiveGroup ((^+^), (^-^))
import Data.Map (Map)
import Data.VectorSpace ((^*), normalized)
import Data.Maybe (isJust, mapMaybe)
import Data.Traversable (mapM, forM)
import Data.Typeable (Typeable)
import Control.Monad (when, forM_, unless)
import Control.Monad.RWS (execRWST)
import Control.Monad.State (runState)
import Control.Monad.State.Class (MonadState(get, put), modify)
import Control.Monad.Reader.Class (MonadReader(ask), asks)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Obstacles (ObstacleTree)
import Controllers (Controller(..), players)
import GLFWutil (Event(..))
import Util ((.), getDataFileName, whenJust, loadConfig)
import Math
  (V, x_rot_vector, y_rot_vector, tov, Ray(..), StoredVertex, bytesPerObstacle, obstacleTriangles, VisualObstacle, asStoredVertices)
import Logic
  (Player(Player,body), Gun(..), Rope(..), Life(..), positions, birth, GunConfig(shootingRange), collisionPoint, RefreshRate)

import qualified Octree
import qualified Logic
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Pointer as SVP
import qualified Data.Map as Map
import qualified Graphics.UI.GLFW as GLFW
import qualified GLFWutil
import qualified Control.DeepSeq
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.GLU.Raw as GLU


-- Static data:

data Scheme = Scheme
  { shadow_color :: Color4 GLclampf
  , grid_color :: Color4 GLclampf
  , rope_line_width :: GLfloat
  , fog_density :: GLfloat
  , fog_color :: Color4 GLclampf
  , gunColor :: Gun → Color4 GLclampf
  , lightModel_ambient
  , ballLight_ambient, ballLight_diffuse
  , ball_material_ambient, ball_material_diffuse :: Color4 GLfloat
  , ballLight_attenuation :: (GLfloat, GLfloat, GLfloat)
  } deriving Typeable

data GridType =
  DottedGrid { grid_dot_size :: GLfloat } |
  LinedGrid { grid_line_width :: GLfloat }

data FloorConfig = Grid { grid_size :: Integer, grid_type :: GridType }

data CameraConfig = CameraConfig
  { viewing_dist :: GLdouble
  , fov :: GLdouble -- in degrees
  , wheelBounds :: (Int, Int)
  , zoom :: Int → GLdouble
  , mouse_speed :: GLdouble -- in pixels per radian
  , invert_mouse :: Bool
  }

data GunGuiConfig = GunGuiConfig { gun_xrot, gun_yrot :: GLdouble {- in radians -} }

data GuiConfig = GuiConfig
  { windowTitle :: String
  , gunGuiConfig :: Gun → GunGuiConfig
  , ugly :: Bool
  , floorConf :: Maybe FloorConfig
  , playerSize :: GLdouble
  , camConf :: CameraConfig
  , schemeFile :: String
  , restart_key, pause_key, exit_key :: GLFW.Key
  , gunForButton :: GLFW.MouseButton → Maybe Gun
  } deriving Typeable

data Static = Static
  { obstacleBuffer :: GL.BufferObject
  , bodyQuadric :: Ptr GLU.GLUquadric
  , scheme :: Scheme
  , guiConfig :: GuiConfig
  , gunConfig :: Gun → GunConfig -- not part of GuiConfig because gunConfig is normally read from a gameplay config file
  , vertexCount :: Int
  , tree :: ObstacleTree
  , poll :: IO [Event] }


-- Dynamic data:

data CameraOrientation = CameraOrientation { cam_dist, cam_xrot, cam_yrot :: !GLdouble } deriving Show
data FireState = FireAsap | ReleaseAsap | Fired | Idle
data ClientGunState = ClientGunState { target :: Maybe V, fireState :: FireState }
type Guns = Map Gun ClientGunState
type MousePosition = (Int, Int)

data State c = State
  { controller :: c
  , paused :: Maybe MousePosition
      -- If we're paused, we remember the virtual mouse position so we can restore it when we unpause.
  , camera :: CameraOrientation
  , guns :: Guns }


-- Drawers:

type Drawer a = ∀ m . (MonadReader Static m, MonadIO m) ⇒ m a

rotateRadians :: (Floating c, GL.MatrixComponent c) ⇒ c → Vector3 c → IO ()
rotateRadians r = GL.rotate (r / pi * 180)

ballLight :: GL.Light
ballLight = GL.Light 0

drawEverything :: Controller c ⇒ State c → Drawer ()
drawEverything State{camera=CameraOrientation{..}, ..} = do
  liftIO $ do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.loadIdentity
    GL.translate $ Vector3 0 0 (- cam_dist)
    GL.lighting $= Enabled
    GL.position ballLight $= Vertex4 30 30 100 1
    rotateRadians cam_xrot $ Vector3 1 0 0
    rotateRadians cam_yrot $ Vector3 0 1 0

  whenJust (birth $ player controller) $ \me → do
  liftIO $ GL.translate $ (rayOrigin $ body me) ^* (-1) -- todo
  drawPlayers $ mapMaybe birth $ players controller
  drawObstacles
  liftIO $ GL.lighting $= Disabled
  --lift $ drawFutures players
  drawFloor me
  drawRopes $ mapMaybe birth $ players controller
  drawCrossHairs guns

drawFloor :: Player → Drawer ()
drawFloor Player{..} = do
  Scheme{..} ← asks scheme
  GuiConfig{camConf=CameraConfig{..}, ..} ← asks guiConfig
  liftIO $ do
  whenJust floorConf $ \kind → do
  case kind of
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
          GL.renderPrimitive GL.Lines $
            forM_ [-vd, -vd + (fromInteger grid_size) .. vd] $ \n →
              mapM (GL.vertex . tov) $
                [ Vector3 (aligned_x + n) 0 (z - vd), Vector3 (aligned_x + n) 0 (z + vd)
                , Vector3 (x - vd) 0 (aligned_z + n), Vector3 (x + vd) 0 (aligned_z + n) ]
        DottedGrid{..} → do
          GL.pointSize $= grid_dot_size
          GL.renderPrimitive GL.Points $
            forM_ [(aligned_x + x', aligned_z + z') | x' ← [-vd, -vd + (fromInteger grid_size) .. vd], z' ← [-vd, -vd + (fromInteger grid_size) .. vd]] $ \(x', z') →
              GL.vertex $ tov $ Vector3 x' 0 z'

drawCrossHairs :: Guns → Drawer ()
drawCrossHairs guns = do
  scheme ← asks scheme
  guiConfig ← asks guiConfig
  liftIO $ do
  GL.lineWidth $= 3
  GL.pointSize $= 4
  forM_ (Map.toList guns) $ \(g, gu) → do
    let GunGuiConfig{..} = gunGuiConfig guiConfig g
    GL.color $ gunColor scheme g
    GL.loadIdentity
    rotateRadians gun_xrot $ Vector3 (-1) 0 0
    rotateRadians gun_yrot $ Vector3 0 (-1) 0
    GL.renderPrimitive GL.Points $ GL.vertex $ Vertex3 (0 :: GLdouble) 0 (-100)
    when (isJust $ target gu) $ GL.renderPrimitive GL.LineLoop $ mapM_ GL.vertex
      [ Vertex3 (-1 :: GLdouble) 0 (-100)
      , Vertex3 (0 :: GLdouble) (-1) (-100)
      , Vertex3 (1 :: GLdouble) 0 (-100)
      , Vertex3 (0 :: GLdouble) 1 (-100) ]

drawRopes :: [Player] → Drawer ()
drawRopes ps = do
  Scheme{..} ← asks scheme
  GuiConfig{..} ← asks guiConfig
  liftIO $ do
  GL.lineWidth $= rope_line_width
  GL.renderPrimitive GL.Lines $ forM ps $ \p@Player{..} →
    forM_ (Map.toList (Logic.guns p)) $ \(gun, Rope{..}) → do
        GL.color $ gunColor gun
        GL.vertex $ tov $ rayOrigin body ^+^ (normalized (rayOrigin rope_ray ^-^ rayOrigin body) ^* (playerSize + 0.05))
        GL.vertex $ tov $ rayOrigin rope_ray
  return ()

drawObstacles :: Drawer ()
drawObstacles = do
  Static{scheme=Scheme{..}, ..} ← ask
  liftIO $ do

  GL.materialDiffuse GL.Front $= Color4 1 1 1 1
  GL.materialAmbient GL.Front $= Color4 0.4 0.6 0.8 1
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

  GL.drawArrays GL.Triangles 0 (fromIntegral vertexCount)
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  GL.clientState GL.VertexArray $= Disabled
  GL.clientState GL.NormalArray $= Disabled
  GL.clientState GL.ColorArray $= Disabled

drawPlayers :: [Player] → Drawer ()
drawPlayers p = do
  Static{scheme=Scheme{..}, guiConfig=GuiConfig{..}, bodyQuadric} ← ask
  liftIO $ do
  GL.materialAmbient GL.Front $= ball_material_ambient
  GL.materialDiffuse GL.Front $= ball_material_diffuse
  forM p $ \Player{..} → GL.preservingMatrix $ do
    GL.translate $ rayOrigin body
    GLU.gluSphere bodyQuadric playerSize 20 20
  return ()

drawFutures :: [Life] → IO ()
drawFutures ls = do
  GL.color green
  forM_ ls $ GL.renderPrimitive GL.LineStrip . mapM_ (GL.vertex . tov) . take 500 . positions

green :: Color4 GLclampf
green = Color4 0 1 0 1


-- Mechanics:

initialize :: [VisualObstacle] → ObstacleTree → GuiConfig → (Gun → GunConfig) → GLdouble →
  (RefreshRate → c) → IO (Static, State c)
initialize obstacles tree guiConfig@GuiConfig{..} gunConfig initialCamYrot initialController = do

  scheme@Scheme{..} :: Scheme
    ← getDataFileName "schemes" >>= loadConfig . (++ "/" ++ schemeFile)

  True ← GLFW.initialize

  rr ← GLFWutil.getWindowRefreshRate

  True ← GLFW.openWindow GLFW.defaultDisplayOptions
    { GLFW.displayOptions_numRedBits = 8
    , GLFW.displayOptions_numGreenBits = 8
    , GLFW.displayOptions_numBlueBits = 8
    , GLFW.displayOptions_numDepthBits = 1
    }

  GLFW.disableAutoPoll
  poll ← GLFWutil.prepareListPoll

  GL.depthFunc $= Just GL.Lequal
  GL.clearColor $= fog_color
  GL.cullFace $= Just GL.Back
  GL.lineSmooth $= if ugly then Disabled else Enabled
  GL.pointSmooth $= if ugly then Disabled else Enabled
  GL.normalize $= if ugly then Disabled else Enabled
  GL.shadeModel $= if ugly then GL.Flat else GL.Smooth
  GL.hint GL.LineSmooth $= if ugly then GL.Fastest else GL.Nicest
  GL.hint GL.PointSmooth $= if ugly then GL.Fastest else GL.Nicest
  GL.fog $= Enabled
  GL.fogMode $= GL.Exp2 fog_density
  GL.fogColor $= fog_color
  GL.lightModelAmbient $= lightModel_ambient
  GL.light ballLight $= Enabled
  GL.ambient ballLight $= ballLight_ambient
  GL.diffuse ballLight $= ballLight_diffuse
  GL.attenuation ballLight $= ballLight_attenuation
  GL.colorMaterial $= Just (GL.FrontAndBack, GL.AmbientAndDiffuse)

  [obstacleBuffer] ← GL.genObjectNames 1
  let size = fromIntegral (length obstacles) * bytesPerObstacle
  GL.bindBuffer GL.ArrayBuffer $= Just obstacleBuffer
  let vertices = asStoredVertices obstacles
  GL.bufferData GL.ArrayBuffer $= (size, SVP.ptr (SVP.cons vertices), GL.StaticDraw)

  bodyQuadric ← GLU.gluNewQuadric

  GLFW.setWindowBufferSwapInterval 1

  return
    ( Static
      { vertexCount = SV.length vertices, .. }
    , State
      { controller = initialController (fromIntegral rr)
      , paused = Just (0, 0)
      , camera = CameraOrientation (zoom camConf 0) 0 initialCamYrot
      , guns = Map.fromList
          [ (LeftGun, ClientGunState Nothing Idle)
          , (RightGun, ClientGunState Nothing Idle) ] } )

gui :: Controller c ⇒ [VisualObstacle] → ObstacleTree → GuiConfig → (Gun → GunConfig) → GLdouble →
  (RefreshRate → c) → IO c
gui obstacles tree guiConfig gunConfig initialCamYrot initialController = do
  Control.DeepSeq.deepseq tree $ do
  (r, ()) ← initialize obstacles tree guiConfig gunConfig initialCamYrot initialController
    >>= uncurry (execRWST loop)
  GLFW.closeWindow
  GLFW.terminate
  return $ controller r

loop :: (Controller c, MonadReader Static m, MonadState (State c) m, MonadIO m) ⇒ m ()
loop = do
  get >>= drawEverything
  p ← asks poll
  liftIO (GLFW.swapBuffers >> p) >>= mapM onEvent
  s ← get
  when (paused s == Nothing) $ whenJust (birth $ player (controller s)) $ \Player{body} → do
    static ← ask
    let
      (newGuns, newController) =
        runState (Map.traverseWithKey (fireGuns static (camera s) (rayOrigin body)) (guns s)) (controller s)
    put s{controller=tick newController, guns=newGuns}
  b ← asks (exit_key . guiConfig) >>= liftIO . GLFW.keyIsPressed
  o ← liftIO $ GLFW.windowIsOpen
  unless (b || not o) loop

onEvent :: (MonadReader Static m, MonadState (State c) m, MonadIO m) ⇒ Event → m ()
onEvent e = do
  Static{guiConfig=GuiConfig{camConf=CameraConfig{..}, ..}, ..} ← ask
  case e of
    MouseButtonEvent but b → whenJust (gunForButton but) $ \g → modify $ \s →
        s{guns=Map.adjust (\gs → gs { fireState = if b then FireAsap else ReleaseAsap }) g (guns s)}
    MouseWheelEvent p
      | p < fst wheelBounds → liftIO $ GLFW.setMouseWheel $ fst wheelBounds
      | p > snd wheelBounds → liftIO $ GLFW.setMouseWheel $ snd wheelBounds
      | otherwise → modify $ \s → s { camera = (camera s){cam_dist = zoom p} }
    MousePositionEvent x y → do
      s ← get
      when (paused s == Nothing) $ put s { camera = (camera s){
        cam_xrot = (if invert_mouse then negate else id) (fromIntegral y / mouse_speed),
        cam_yrot = fromIntegral x / mouse_speed} }
    KeyEvent k True | k == pause_key → do
      s ← get
      case paused s of
        Just p → do
          liftIO $ GLFW.disableMouseCursor >> uncurry GLFW.setMousePosition p
          put s{paused=Nothing}
        Nothing → do
          p ← liftIO GLFW.getMousePosition
          liftIO GLFW.enableMouseCursor
          put s{paused=Just p}
    WindowSizeEvent w h → liftIO $ do
      GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GLU.gluPerspective fov (fromIntegral w / fromIntegral h) 10 viewing_dist
      GL.matrixMode $= GL.Modelview 0
    _ → return ()

fireGuns :: (Controller c, MonadState c m) ⇒ Static → CameraOrientation → V → Gun → ClientGunState → m ClientGunState
fireGuns Static{..} o playerPos g ClientGunState{..} = do
    c ← get
    newFireState ← case (newTarget, fireState) of
      (Just t, FireAsap) | Just c' ← fire g (Just t) c → put c' >> return Fired
      (_, ReleaseAsap) | Just c' ← fire g Nothing c → put c' >> return Idle
      _ → return fireState
    return ClientGunState{target=newTarget, fireState=newFireState}
  where
    r = gunRay o playerPos (gunConfig g) (gunGuiConfig guiConfig g)
    newTarget = collisionPoint r (Octree.query r tree >>= obstacleTriangles)

gunDirection :: CameraOrientation → GunGuiConfig → V
gunDirection CameraOrientation{..} GunGuiConfig{..} = Vector3 0 0 (-1)
  `x_rot_vector` (gun_xrot + cam_xrot)
  `y_rot_vector` (gun_yrot + cam_yrot)

cameraOffset :: CameraOrientation → V
cameraOffset CameraOrientation{..} =
  Vector3 0 0 (- cam_dist) `x_rot_vector` cam_xrot `y_rot_vector` cam_yrot

gunRay :: CameraOrientation → V → GunConfig → GunGuiConfig → Ray
gunRay c playerPos d g = Ray (playerPos ^-^ cameraOffset c) (gunDirection c g ^* shootingRange d)
