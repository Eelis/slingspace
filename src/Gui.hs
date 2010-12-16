{-# LANGUAGE RecordWildCards #-}

module Gui (GuiCallback(..), gui) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (Vector3(..), GLdouble, ($=), Vertex3(..), Vertex4(..), Position(..), vertex, Flavour(..), MouseButton(..), PrimitiveMode(..), GLfloat, Color4, GLclampf, ClearBuffer(..), Face(..), KeyState(..), Normal3(..), Capability(..), Key(..), hint, renderPrimitive)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef, writeIORef)
import Math (V, (<->), (<*>), x_rot_vector, y_rot_vector, tov, wrap, AnnotatedTriangle(..))
import Maybe (isJust)
import Control.Monad (when, unless, forM_)
import Data.Traversable (forM)
import Control.Monad.Fix()
import Logic (Player(..), PlayerBody(..), Gun(..), GameplayConfig(..), Rope(..), find_target, aos_to_ats, obstacles_around)
import MyGL ()
import System.Exit (exitWith, ExitCode(ExitSuccess))
import MyUtil ((.), getDataFileName, read_config_file, timeofday_usecs)
import Prelude hiding ((.))

data Scheme = Scheme
  { shadow_color :: Color4 GLclampf
  , grid_color :: Color4 GLclampf
  , rope_line_width :: GLfloat
  , fog_density :: GLfloat
  , fog_color, left_gun_color, right_gun_color :: Color4 GLclampf
  , lightModel_ambient
  , obstacle_material_ambient, obstacle_material_diffuse
  , ballLight_ambient, ballLight_diffuse
  , ball_material_ambient, ball_material_diffuse :: Color4 GLfloat
  , ballLight_attenuation :: (GLfloat, GLfloat, GLfloat)
  } deriving Read

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
  , mouse_speed :: GLdouble -- in pixels per degree
  , invert_mouse :: Bool
  } deriving Read

data GunConfig = GunConfig { gun_xrot, gun_yrot :: GLdouble, gun_color :: Color4 GLclampf }

data GuiConfig = GuiConfig
  { window_title :: String
  , cross_offset_hor, cross_offset_ver :: GLdouble -- in degrees
  , ugly :: Bool
  , floor_conf :: Maybe FloorConfig
  , player_size :: GLdouble
  , cam_conf :: CameraConfig
  , schemeFile :: String
  , restart_key, pause_key, exit_key, zoom_in_key, zoom_out_key :: (Key, KeyState)
  } deriving Read

-- state

data Camera = Camera { cam_dist, cam_xrot, cam_yrot :: !GLdouble }

data FireState = FireAsap | Fired | Idle

data ClientGunState = ClientGunState { target :: Maybe V, fire_state :: FireState }

type ClientState = Map Gun ClientGunState

class GuiCallback c where
  cc_tick :: c → IO ()
  cc_players :: c → IO (Map String Player)
  cc_release :: c → Gun → IO ()
  cc_fire :: c → Gun → V → IO ()
  cc_spawn :: c → IO ()

gunConfig :: Scheme → GuiConfig → Gun → GunConfig
gunConfig sch cf g =
  case g of
    LeftGun → GunConfig (- cross_offset_ver cf) (- cross_offset_hor cf) (left_gun_color sch)
    RightGun → GunConfig (- cross_offset_ver cf) (cross_offset_hor cf) (right_gun_color sch)

initialClientState :: ClientState
initialClientState = Map.fromList $ flip (,) (ClientGunState Nothing Idle) . [LeftGun, RightGun]

gui :: GuiCallback c ⇒ c → String → GameplayConfig → IO ()
gui c name gameplayConfig = do

  guiConfig :: GuiConfig
    ← read_config_file "gui.txt"
  scheme :: Scheme
    ← read . (readFile =<< (++ "/" ++ schemeFile guiConfig) . getDataFileName "schemes")
  clientState ← newIORef initialClientState

  GLUT.getArgsAndInitialize

  GLUT.initialDisplayMode $= [GLUT.DoubleBuffered, GLUT.WithDepthBuffer, GLUT.WithAlphaComponent, GLUT.RGBAMode]
  GLUT.createWindow $ window_title guiConfig
  GLUT.depthFunc $= Just GLUT.Lequal -- Less
  GLUT.clearColor $= fog_color scheme
  GLUT.lineWidth $= 3 -- todo: anders
  GLUT.pointSize $= 4 -- todo: anders
  GLUT.cullFace $= Just Back

  if ugly guiConfig
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
  GLUT.fogMode $= GLUT.Exp2 (fog_density scheme)
  GLUT.fogColor $= fog_color scheme
  GLUT.lightModelAmbient $= lightModel_ambient scheme
  GLUT.light ballLight $= Enabled
  GLUT.ambient ballLight $= ballLight_ambient scheme
  GLUT.diffuse ballLight $= ballLight_diffuse scheme
  GLUT.attenuation ballLight $= ballLight_attenuation scheme

  GLUT.blend $= Enabled
  GLUT.blendFunc $= (GLUT.SrcAlpha, GLUT.OneMinusSrcAlpha)

  setup_glut_callbacks c guiConfig scheme clientState name gameplayConfig

  GLUT.mainLoop


-- glut app

ballLight :: GLUT.Light
ballLight = GLUT.Light 0


onMotion :: CameraConfig → IORef Camera → IORef Position → Position → IO ()
onMotion cam_conf@CameraConfig{..} cameraRef cursorPosRef =
  motion cam_conf (\ x y → modifyIORef cameraRef (\c → c { cam_xrot = cam_xrot c + x, cam_yrot = cam_yrot c + y }) {->> postRedisplay Nothing-}) cursorPosRef

togglePause :: CameraConfig → IORef Bool → IORef Camera → IORef Position → IO ()
togglePause cam_conf pauseRef cameraRef cursorPosRef = do
    p ← readIORef pauseRef
    writeIORef pauseRef $ not p
    let (c, mc) = if p then (GLUT.None, Just $ onMotion cam_conf cameraRef cursorPosRef) else (GLUT.Inherit, Nothing)
    GLUT.cursor $= c
    GLUT.motionCallback $= mc
    GLUT.passiveMotionCallback $= mc

onInput :: GuiCallback c ⇒ c → GuiConfig → IORef ClientState → IORef Bool → IORef Camera → IORef Position → Key → KeyState → a → b → IO ()
onInput cc GuiConfig{cam_conf=cam_conf@CameraConfig{..}, ..} clientStateRef pauseRef cameraRef cursorPos b bs _ _ =
  case () of
    _| k == restart_key → cc_spawn cc
    _| k == pause_key → togglePause cam_conf pauseRef cameraRef cursorPos
    _| k == exit_key → exitWith ExitSuccess
    _| k == zoom_in_key →
      modifyIORef cameraRef $ \cam → cam { cam_dist = max (cam_dist cam / cam_zoom_speed) cam_min_dist }
    _| k == zoom_out_key →
      modifyIORef cameraRef $ \cam → cam { cam_dist = min (cam_dist cam * cam_zoom_speed) cam_max_dist }
    _| otherwise → case (b, bs) of
     (MouseButton LeftButton, Down) → fire_asap_a LeftGun
     (MouseButton LeftButton, Up) → release_a LeftGun
     (MouseButton RightButton, Down) → fire_asap_a RightGun
     (MouseButton RightButton, Up) → release_a RightGun
     _ → return ()
 where
  k = (b, bs)
  fire_asap_a = modifyIORef clientStateRef . Map.adjust (\g → g { fire_state = FireAsap })
  release_a u = modifyIORef clientStateRef (Map.adjust (\g → g { fire_state = Idle }) u) >> cc_release cc u

setup_glut_callbacks :: GuiCallback c ⇒
  c → GuiConfig → Scheme → IORef ClientState → String → GameplayConfig → IO ()
setup_glut_callbacks
 cc guiConfig@GuiConfig{cam_conf=cam_conf@CameraConfig{..}, ..} scheme
 clientStateRef name gameplayConfig = do

  pauseRef ← newIORef True
  cursorPos ← newIORef $ Position 0 0
  cameraRef ← newIORef $ Camera cam_init_dist 0 0
  tr ← timeofday_usecs >>= newIORef

  GLUT.reshapeCallback $= Just (reshape cam_conf)
  GLUT.displayCallback $= do
    camera ← readIORef cameraRef
    clientState ← readIORef clientStateRef
    display cc guiConfig scheme name camera clientState
  GLUT.keyboardMouseCallback $= Just (onInput cc guiConfig clientStateRef pauseRef cameraRef cursorPos)
  GLUT.idleCallback $= Just (do
    t ← readIORef tr
    tn ← timeofday_usecs
    when (tn >= t) $ do
      writeIORef tr (t + 10 * 1000) -- Once every 10 ms. Todo: Make configurable etc.
      actual_tick pauseRef cc cameraRef scheme guiConfig clientStateRef name gameplayConfig
      GLUT.postRedisplay Nothing)

whenJust :: Monad m ⇒ Maybe a → (a → m ()) → m ()
whenJust = flip (maybe (return ()))

actual_tick :: GuiCallback c ⇒ IORef Bool → c → IORef Camera → Scheme → GuiConfig → IORef ClientState → String → GameplayConfig → IO ()
actual_tick pauseRef cc cameraRef scheme guiConfig clientStateRef myname gameplayConfig = do
  p ← readIORef pauseRef
  unless p $ do
    -- errs ← get errors
    -- print $ "[" ++ (show errs) ++ "]"

  maybePlayer ← Map.lookup myname . cc_players cc

  whenJust maybePlayer $ \player@Player{..} → do

  Camera{..} ← readIORef cameraRef

  let cam_pos = pb_pos body <-> (Vector3 0 0 (- cam_dist) `x_rot_vector` (cam_xrot / 180 * pi) `y_rot_vector` (cam_yrot / 180 * pi))

  clientState ← readIORef clientStateRef

  writeIORef clientStateRef $ flip Map.mapWithKey clientState $ \k → (\t g → g { target = t }) $
    let GunConfig{..} = gunConfig scheme guiConfig k
    in find_target player gameplayConfig cam_pos $ Vector3 0 0 (-1)
      `x_rot_vector` (gun_xrot / 180 * pi)
      `y_rot_vector` (gun_yrot / 180 * pi)
      `x_rot_vector` (cam_xrot / 180 * pi)
      `y_rot_vector` (cam_yrot / 180 * pi)

  forM_ (Map.toList clientState) $ \(k, v) →
    case v of
      ClientGunState (Just t) FireAsap → do
        cc_fire cc k t
        modifyIORef clientStateRef $ Map.adjust (\g → g { fire_state = Fired }) k
      _ → return ()

  cc_tick cc

draw_floor :: Scheme → CameraConfig → [AnnotatedTriangle] → Player → FloorConfig → IO ()
draw_floor Scheme{..} CameraConfig{..} visible_obs me kind = case kind of
  Shadows → do
      GLUT.color shadow_color
      GLUT.renderPrimitive Triangles $ forM_ visible_obs $
        \(AnnotatedTriangle _ (Vector3 ax _ az, Vector3 bx _ bz, Vector3 cx _ cz) _ _) → do
          vertex $ Vertex3 ax 0 az
          vertex $ Vertex3 bx 0 bz
          vertex $ Vertex3 cx 0 cz
  Grid gs gt → do
      GLUT.color grid_color
      let
        Vector3 x _ z = pb_pos $ body me
        funky h = fromInteger (h' - (h' `mod` gs)) :: GLdouble where h' = round h :: Integer
        aligned_z = funky z; aligned_x = funky x
        vd = viewing_dist
      case gt of
        LinedGrid lw → do
          GLUT.lineWidth $= lw
          renderPrimitive Lines $
            forM_ [-vd, -vd + (fromInteger gs) .. vd] $ \n →
              mapM (vertex . tov) $
                [ Vector3 (aligned_x + n) 0 (z - vd), Vector3 (aligned_x + n) 0 (z + vd)
                , Vector3 (x - vd) 0 (aligned_z + n), Vector3 (x + vd) 0 (aligned_z + n) ]
        DottedGrid gds → do
          GLUT.pointSize $= gds
          GLUT.renderPrimitive Points $
            forM_ [(aligned_x + x', aligned_z + z') | x' ← [-vd, -vd + (fromInteger gs) .. vd], z' ← [-vd, -vd + (fromInteger gs) .. vd]] $ \(x', z') →
              vertex $ tov $ Vector3 x' 0 z'



display :: GuiCallback c ⇒ c → GuiConfig → Scheme → String → Camera → ClientState → IO ()
display cc guiConfig@GuiConfig{..} scheme@Scheme{..} myname Camera{..} clientState = do
  GLUT.clear [ColorBuffer, DepthBuffer]
  GLUT.loadIdentity
  GLUT.translate $ Vector3 0 0 (- cam_dist)

  GLUT.lighting $= Enabled
  GLUT.position ballLight $= Vertex4 30 30 100 1
  GLUT.rotate cam_xrot $ Vector3 1 0 0
  GLUT.rotate cam_yrot $ Vector3 0 1 0

  players ← cc_players cc

  whenJust (Map.lookup myname players) $ \me → do

  GLUT.translate $ (pb_pos $ body me) <*> (-1)

  -- draw players:

  GLUT.materialAmbient Front $= ball_material_ambient
  GLUT.materialDiffuse Front $= ball_material_diffuse
  forM players $ \player → GLUT.preservingMatrix $ do
    GLUT.translate $ pb_pos $ body player
    GLUT.renderObject Solid $ GLUT.Sphere' player_size 20 20

  -- draw obstacles:

  let visible_obs = aos_to_ats $ take 400 $ obstacles_around me

  GLUT.materialAmbient Front $= obstacle_material_ambient
  GLUT.materialDiffuse Front $= obstacle_material_diffuse
  GLUT.renderPrimitive Triangles $ forM visible_obs $
   \(AnnotatedTriangle (Vector3 nx ny nz) (a, b, c) _ _) → do
    GLUT.normal $ Normal3 nx ny nz; forM [a, b, c] $ vertex . tov

--   materialAmbient Front $= Color4 (1 :: GLfloat) 0 0 0.5
--   renderPrimitive Triangles $ forM (aos_to_ats [gn_obst $ closest_obstacle pl]) $
--    \(AnnotatedTriangle (Vector3 nx ny nz) (a, b, c) _ _) → do
--     normal $ Normal3 nx ny nz; forM [a, b, c] $ vertex . tov

  GLUT.lighting $= Disabled

  whenJust floor_conf $ draw_floor scheme cam_conf visible_obs me

  -- draw ropes:

  GLUT.lineWidth $= rope_line_width
  forM players $ \player →
    forM_ (Map.toList $ guns player) $ \(gun, rope) → do
      GLUT.color $ case gun of LeftGun → left_gun_color; RightGun → right_gun_color
      renderPrimitive Lines $ do
        vertex $ tov $ pb_pos $ body player
        vertex $ tov $ rope_pos rope

  -- draw crosshairs:

  GLUT.lineWidth $= 3
  GLUT.pointSize $= 4
  forM_ (Map.toList clientState) $ \(g, gu) → do
    let GunConfig{..} = gunConfig scheme guiConfig g
    GLUT.color gun_color
    GLUT.loadIdentity
    GLUT.rotate gun_xrot $ Vector3 (-1) 0 0
    GLUT.rotate gun_yrot $ Vector3 0 (-1) 0
    GLUT.renderPrimitive Points $ vertex $ Vertex3 (0 :: GLdouble) 0 (-100)
    when (isJust $ target gu) $ GLUT.renderPrimitive LineLoop $ mapM_ vertex
      [ Vertex3 (-1 :: GLdouble) 0 (-100)
      , Vertex3 (0 :: GLdouble) (-1) (-100)
      , Vertex3 (1 :: GLdouble) 0 (-100)
      , Vertex3 (0 :: GLdouble) 1 (-100) ]

--   t ← readIORef tr
--   tn ← timeofday_usecs
--   putStr $ show tn ++ " "
--   let pluses = (tn - t) `div` 500
--   when (pluses < 100) $ putStrLn $ replicate (fromIntegral pluses) '+'
--   when (pluses >= 100) $ putStrLn "-----"
--   writeIORef tr tn

  GLUT.swapBuffers

reshape :: CameraConfig → GLUT.Size → IO ()
reshape CameraConfig{..} s@(GLUT.Size w h) = do
  GLUT.viewport $= (Position 0 0, s)
  GLUT.matrixMode $= GLUT.Projection
  GLUT.loadIdentity
  GLUT.perspective fov (fromIntegral w / fromIntegral h) 0.1 viewing_dist
  GLUT.matrixMode $= GLUT.Modelview 0

motion :: CameraConfig → (GLdouble → GLdouble → IO ()) → IORef Position → Position → IO ()
motion CameraConfig{..} rot cursorPosRef p@(Position x y) = do
    -- TODO: all of this is sloppy
  Position x' y' ← readIORef cursorPosRef
  let q = Position (wrap x 100 400) (wrap y 100 400)
  writeIORef cursorPosRef q
  when (q /= p) $ GLUT.pointerPosition $= q
  when (abs (x - x') + abs (y - y') < 200) $
    rot
       (let yr = fromIntegral (y - y') / (- mouse_speed)
        in if invert_mouse then -yr else yr)
       (fromIntegral (x - x') / mouse_speed)
