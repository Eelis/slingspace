module Gui (GuiCallback(..), gui) where

import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.UI.GLUT
import Data.IORef
import Math
import Maybe
import Control.Monad
import Control.Monad.Fix()
import Logic
import MyGL ()
import System.Exit
import StaticConfig
import MyUtil
import Prelude hiding ((.))
import GHC.Word

class GuiCallback c where
  cc_tick :: c -> IO ()
  cc_players :: c -> IO (Map String Player)
  cc_release :: c -> Gun -> IO ()
  cc_fire :: c -> Gun -> V -> IO ()
  cc_spawn :: c -> IO ()

gui :: GuiCallback c => c -> String -> GameplayConfig -> IO ()
gui c name gp_cfg = do
  cfg <- read_config_file "gui.txt"
  sch <- read . (readFile =<< (++ "/" ++ schemeFile cfg) . getDataFileName "schemes")
  cs <- newIORef $ Map.fromList $ flip (,) (ClientGunState Nothing Idle) . [LeftGun, RightGun]

  getArgsAndInitialize

  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer, WithAlphaComponent, RGBAMode]
  createWindow $ window_title cfg
  depthFunc $= Just Lequal -- Less
  clearColor $= fog_color sch
  lineWidth $= 3 -- todo: anders
  pointSize $= 4 -- todo: anders
  cullFace $= Just Back

  if ugly cfg
   then do
    lineSmooth $= Disabled
    pointSmooth $= Disabled
    normalize $= Disabled
    shadeModel $= Flat
    hint LineSmooth $= Fastest
    hint PointSmooth $= Fastest
   else do
    lineSmooth $= Enabled
    pointSmooth $= Enabled
    shadeModel $= Smooth
    hint LineSmooth $= Nicest
    hint PointSmooth $= Nicest

  fog $= Enabled
  fogMode $= Exp2 (fog_density sch)
  fogColor $= fog_color sch
  lightModelAmbient $= lightModel_ambient sch
  light ballLight $= Enabled
  ambient ballLight $= ballLight_ambient sch
  diffuse ballLight $= ballLight_diffuse sch
  attenuation ballLight $= ballLight_attenuation sch

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  setup_glut_callbacks c cfg sch cs name gp_cfg

  mainLoop

-- config

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

data FloorConfig = Grid { grid_size :: Integer, grid_type :: GridType } | Shadows | NoFloor deriving Read

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
  , floor_conf :: FloorConfig
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

-- glut app

ballLight :: Light
ballLight = Light 0

--gun_configs' :: Scheme -> GuiConfig -> Map Gun GunConfig

gun_config :: Scheme -> GuiConfig -> Gun -> GunConfig
gun_config sch cf g =
  case g of
    LeftGun -> GunConfig (- cross_offset_ver cf) (- cross_offset_hor cf) (left_gun_color sch)
    RightGun -> GunConfig (- cross_offset_ver cf) (cross_offset_hor cf) (right_gun_color sch)

setup_glut_callbacks :: GuiCallback c =>
  c -> GuiConfig -> Scheme -> IORef ClientState -> String -> GameplayConfig -> IO ()
setup_glut_callbacks cc cfg sch cs name gp_cfg = do
  let cam_cfg = cam_conf cfg

  pauseRef <- newIORef True

  cursorPos <- newIORef $ Position 0 0
  cameraRef <- newIORef $ Camera (cam_init_dist cam_cfg) 0 0

  reshapeCallback $= Just (reshape cam_cfg)

  let
   mot = motion cam_cfg (\ x y -> modifyIORef cameraRef (\c -> c { cam_xrot = cam_xrot c + x, cam_yrot = cam_yrot c + y }) {->> postRedisplay Nothing-}) cursorPos

   toggle_pause_act = do
    p <- readIORef pauseRef
    writeIORef pauseRef $ not p
    let (c, mc) = if p then (None, Just mot) else (Inherit, Nothing)
    cursor $= c
    motionCallback $= mc
    passiveMotionCallback $= mc

  tr <- newIORef 0

  displayCallback $= do
    cam <- readIORef cameraRef
    cst <- readIORef cs
    display tr cc cfg sch name cam cst

  keyboardMouseCallback $= (Just $ \b bs _ _ -> do
    let
     k = (b, bs)
     fire_asap_a = modifyIORef cs . Map.adjust (\g -> g { fire_state = FireAsap })
     release_a u = modifyIORef cs (Map.adjust (\g -> g { fire_state = Idle }) u) >> cc_release cc u
    if k == restart_key cfg then cc_spawn cc
     else if k == pause_key cfg then toggle_pause_act
     else if k == exit_key cfg then exitWith ExitSuccess
     else if k == zoom_in_key cfg then
      modifyIORef cameraRef $ \cam -> cam { cam_dist = max (cam_dist cam / (cam_zoom_speed cam_cfg)) (cam_min_dist cam_cfg) }
     else if k == zoom_out_key cfg then
      modifyIORef cameraRef $ \cam -> cam { cam_dist = min (cam_dist cam * (cam_zoom_speed cam_cfg)) (cam_max_dist cam_cfg) }
     else
      case (b, bs) of
       (MouseButton LeftButton, Down) -> fire_asap_a LeftGun
       (MouseButton LeftButton, Up) -> release_a LeftGun
       (MouseButton RightButton, Down) -> fire_asap_a RightGun
       (MouseButton RightButton, Up) -> release_a RightGun
       _ -> return ()
   )

  
  (idleCallback $=) $ Just $ do
  --do_idly_every_n_msecs tick_duration $ do
    --t <- readIORef tr
    --tn <- timeofday_usecs
    --let pluses = (tn - t) `div` 500
    --when (pluses < 100) $ putStrLn $ take (fromIntegral pluses) $ repeat '+'
    --writeIORef tr tn
    postRedisplay Nothing
    actual_tick pauseRef cc cameraRef sch cfg cs name gp_cfg


do_idly_every_n_msecs :: Word64 -> IO () -> IO ()
do_idly_every_n_msecs n a = do
  now <- timeofday_usecs
  mr <- newIORef (now :: Word64)
  idleCallback $= Just (do
    u <- timeofday_usecs
    m <- readIORef mr
    when (u > m) $ a >> writeIORef mr (m + n * 1000)
    )


do_every_n_msecs_simple :: Integer -> IO () -> IO ()
do_every_n_msecs_simple n a = do_every_n_msecs n () (const a)

do_every_n_msecs :: Integer -> a -> (a -> IO a) -> IO ()
do_every_n_msecs n x a = timeofday_usecs >>= w x
  where
    w y p = do
      u <- timeofday_usecs
      if (u > p)
        then do
          x' <- a y
          w x' (p + fromIntegral n * 1000)
        else addTimerCallback 1 $ w y p

--         print (p - u) else putStrLn $ "-" ++ show (u - p)
--       
--       let p' = p + fromIntegral n * 1000; next = w x' p'
--       q <- timeofday_usecs
--       if p' > q then addTimerCallback (fromIntegral $ (p' - q) `quot` 1000) next else next


-- do_every_n_msecs :: Integer -> a -> (a -> IO a) -> IO ()
-- do_every_n_msecs n x a = timeofday_usecs >>= w x
--   where
--     w y p = do
--       u <- timeofday_usecs
--       if (u < p) then print (p - u) else putStrLn $ "-" ++ show (u - p)
--       x' <- a y
--       let p' = p + fromIntegral n * 1000; next = w x' p'
--       q <- timeofday_usecs
--       if p' > q then addTimerCallback (fromIntegral $ (p' - q) `quot` 1000) next else next

actual_tick :: GuiCallback c => IORef Bool -> c -> IORef Camera -> Scheme -> GuiConfig -> IORef ClientState -> String -> GameplayConfig -> IO ()
actual_tick pauseRef cc cameraRef sch cfg cs myname gp_cfg = do
  p <- readIORef pauseRef
  unless p $ do
    -- errs <- get errors
    -- print $ "[" ++ (show errs) ++ "]"

  pls <- cc_players cc

  (flip $ maybe (return ())) (Map.lookup myname pls) $ \pl -> do

  cam <- readIORef cameraRef

  let cam_pos = pb_pos (body pl) <-> (Vector3 0 0 (- cam_dist cam) `x_rot_vector` (cam_xrot cam / 180 * pi) `y_rot_vector` (cam_yrot cam / 180 * pi))

  cst <- readIORef cs

  writeIORef cs $ flip Map.mapWithKey cst $ \k -> (\t g -> g { target = t }) $
    let GunConfig xr yr _ = gun_config sch cfg k
    in find_target pl gp_cfg cam_pos $ Vector3 0 0 (-1)
      `x_rot_vector` (xr / 180 * pi)
      `y_rot_vector` (yr / 180 * pi)
      `x_rot_vector` (cam_xrot cam / 180 * pi)
      `y_rot_vector` (cam_yrot cam / 180 * pi)

  forM_ (Map.toList cst) $ \(k, v) ->
    case v of
      ClientGunState (Just t) FireAsap -> do
        cc_fire cc k t
        modifyIORef cs $ Map.adjust (\g -> g { fire_state = Fired }) k
      _ -> return ()

  cc_tick cc


display :: GuiCallback c => IORef GHC.Word.Word64 -> c -> GuiConfig -> Scheme -> String -> Camera -> ClientState -> IO ()
display tr cc cfg sch myname cam cs = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  translate $ Vector3 0 0 (- cam_dist cam)

  lighting $= Enabled
  position ballLight $= Vertex4 30 30 100 1
  rotate (cam_xrot cam) $ Vector3 1 0 0
  rotate (cam_yrot cam) $ Vector3 0 1 0

  pls <- cc_players cc

  (flip $ maybe (return ())) (Map.lookup myname pls) $ \pl -> do

  translate $ (pb_pos $ body pl) <*> (-1)

  -- draw players:

  materialAmbient Front $= ball_material_ambient sch
  materialDiffuse Front $= ball_material_diffuse sch
  forM_ (Map.toList pls) $ \(_, pla) -> preservingMatrix $ do
    translate $ pb_pos $ body pla
    renderObject Solid $ Sphere' (player_size cfg) 20 20

  -- draw obstacles:

  let visible_obs = aos_to_ats $ take 400 $ obstacles_around pl

  materialAmbient Front $= obstacle_material_ambient sch
  materialDiffuse Front $= obstacle_material_diffuse sch
  renderPrimitive Triangles $ forM visible_obs $
   \(AnnotatedTriangle (Vector3 nx ny nz) (a, b, c) _ _) -> do
    normal $ Normal3 nx ny nz; forM [a, b, c] $ vertex . tov

--   materialAmbient Front $= Color4 (1 :: GLfloat) 0 0 0.5
--   renderPrimitive Triangles $ forM (aos_to_ats [gn_obst $ closest_obstacle pl]) $
--    \(AnnotatedTriangle (Vector3 nx ny nz) (a, b, c) _ _) -> do
--     normal $ Normal3 nx ny nz; forM [a, b, c] $ vertex . tov

  -- draw floor:

  lighting $= Disabled

  case floor_conf cfg of
    NoFloor -> return ()
    Shadows -> do
      color $ shadow_color $ sch
      renderPrimitive Triangles $ forM_ visible_obs $
        \(AnnotatedTriangle _ (Vector3 ax _ az, Vector3 bx _ bz, Vector3 cx _ cz) _ _) -> do
          vertex $ Vertex3 ax 0 az
          vertex $ Vertex3 bx 0 bz
          vertex $ Vertex3 cx 0 cz
    Grid gs gt -> do
      color $ grid_color sch
      let
        Vector3 x _ z = pb_pos $ body pl
        funky h = fromInteger (h' - (h' `mod` gs)) :: GLdouble where h' = round h :: Integer
        aligned_z = funky z; aligned_x = funky x
        vd = viewing_dist $ cam_conf cfg
      case gt of
        LinedGrid lw -> do
          lineWidth $= lw
          renderPrimitive Lines $
            forM_ [-vd, -vd + (fromInteger gs) .. vd] $ \n ->
              mapM (vertex . tov) $
                [ Vector3 (aligned_x + n) 0 (z - vd), Vector3 (aligned_x + n) 0 (z + vd)
                , Vector3 (x - vd) 0 (aligned_z + n), Vector3 (x + vd) 0 (aligned_z + n) ]
        DottedGrid gds -> do
          pointSize $= gds
          renderPrimitive Points $
            forM_ [(aligned_x + x', aligned_z + z') | x' <- [-vd, -vd + (fromInteger gs) .. vd], z' <- [-vd, -vd + (fromInteger gs) .. vd]] $ \(x', z') ->
              vertex $ tov $ Vector3 x' 0 z'

  -- draw ropes:

  lineWidth $= rope_line_width sch
  forM_ (Map.toList pls) $ \(_, pla) ->
    forM_ (Map.toList $ guns pla) $ \(gun, rope) -> do
      color $ (case gun of LeftGun -> left_gun_color; RightGun -> right_gun_color) sch
      renderPrimitive Lines $ do
        vertex $ tov $ pb_pos $ body pla
        vertex $ tov $ rope_pos rope

  -- draw crosshairs:

  lineWidth $= 3
  pointSize $= 4
  forM_ (Map.toList cs) $ \(g, gu) -> do
    let GunConfig xr yr co = gun_config sch cfg g
    color co
    loadIdentity
    rotate xr $ Vector3 (-1) 0 0
    rotate yr $ Vector3 0 (-1) 0
    renderPrimitive Points $ vertex $ Vertex3 (0 :: GLdouble) 0 (-100)
    when (isJust $ target gu) $ renderPrimitive LineLoop $ mapM_ vertex
      [ Vertex3 (-1 :: GLdouble) 0 (-100)
      , Vertex3 (0 :: GLdouble) (-1) (-100)
      , Vertex3 (1 :: GLdouble) 0 (-100)
      , Vertex3 (0 :: GLdouble) 1 (-100) ]

--   t <- readIORef tr
--   tn <- timeofday_usecs
--   putStr $ show tn ++ " "
--   let pluses = (tn - t) `div` 500
--   when (pluses < 100) $ putStrLn $ replicate (fromIntegral pluses) '+'
--   when (pluses >= 100) $ putStrLn "-----"
--   writeIORef tr tn

  swapBuffers

reshape :: CameraConfig -> Size -> IO ()
reshape cfg s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective (fov cfg) (fromIntegral w / fromIntegral h) 0.1 (viewing_dist cfg)
  matrixMode $= Modelview 0

motion :: CameraConfig -> (GLdouble -> GLdouble -> IO ()) -> IORef Position -> Position -> IO ()
motion cfg rot cursorPosRef p@(Position x y) = do
    -- TODO: all of this is sloppy
  Position x' y' <- readIORef cursorPosRef
  let q = Position (wrap x 100 400) (wrap y 100 400)
  writeIORef cursorPosRef q
  when (q /= p) $ pointerPosition $= q
  when (abs (x - x') + abs (y - y') < 200) $
    rot
       (let yr = fromIntegral (y - y') / (- mouse_speed cfg)
        in if invert_mouse cfg then -yr else yr)
       (fromIntegral (x - x') / (mouse_speed cfg))
