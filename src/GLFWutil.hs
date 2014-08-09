{-# LANGUAGE UnicodeSyntax, LambdaCase, ViewPatterns, Rank2Types #-}

module GLFWutil (Event(..), window) where

import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import System.Exit (ExitCode(ExitSuccess))
import qualified Data.Char as Char
import qualified System.Process as Process
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (Window, createWindow)

import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState(get))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.RWS (execRWST)
import Control.Monad (when)


-- The normal GLFW way of responding to events is by registering callbacks that do stuff. Most of these callbacks have an IO() result, meaning that in order to communicate with the rest of the program, they have to use things like IORef, which is ugly. Fortunately, we can implement a general way to communicate these events back to the rest of the program, namely by just recording them in a list and having the poll operation return the list. poll is typically called at the heart of the program loop, which is an excellent place to pass the events on for interpretation and state update, without any need for further IORefs.

data Event
  = WindowSizeEvent Window Int Int
  | WindowRefreshEvent Window
  | CharEvent Window Char
  | KeyEvent Window GLFW.Key Int GLFW.KeyState GLFW.ModifierKeys
  | MouseButtonEvent Window GLFW.MouseButton GLFW.MouseButtonState GLFW.ModifierKeys
  | MousePositionEvent Window Double Double
  | ScrollEvent Window Double Double

prepareListPoll :: Window -> IO (IO [Event])
prepareListPoll w = do
  r ← newIORef []
  let
    push :: Event -> IO ()
    push x = modifyIORef' r (x :)
  GLFW.setWindowRefreshCallback w $ Just $ push . WindowRefreshEvent

  GLFW.setCharCallback w $ Just $ \w' c → push $ CharEvent w' c
  GLFW.setKeyCallback w $ Just $ \w' k i s mk → push $ KeyEvent w' k i s mk
  GLFW.setMouseButtonCallback w $ Just $ \w' m b k → push $ MouseButtonEvent w' m b k
  GLFW.setCursorPosCallback w $ Just $ \w' x y → push $ MousePositionEvent w' x y
  GLFW.setScrollCallback w $ Just $ \w' x y → push $ ScrollEvent w' x y
  GLFW.setWindowSizeCallback w $ Just $ \w' x y → push $ WindowSizeEvent w' x y
    -- beware: glfw "helpfully" calls the window size callback immediately when you set it, so we're already writing to r here

  return $ do
    GLFW.pollEvents
    GLFW.pollEvents
      -- two polls needed because glfw's cursor code is crap
    x ← readIORef r
    writeIORef r []
    return $ reverse x

{-
nvidiaRefreshRate :: IO (Maybe Int)
  -- See https://svn.reviewboard.kde.org/r/5900/
nvidiaRefreshRate =
  Process.readProcessWithExitCode "nvidia-settings" ["-t","-q","RefreshRate"] "" >>= return . \case
    (ExitSuccess, read . takeWhile Char.isDigit → rr, "") → Just rr
    _ → Nothing

getWindowRefreshRate :: IO Int
getWindowRefreshRate =
  nvidiaRefreshRate >>= \case
    Just rr → return rr
    Nothing → GLFWutil.getWindowRefreshRate
-}

window :: ∀ r s .
  (∀ m . (MonadIO m, MonadReader r m) ⇒ s → m ()) →
  (Int → IO (r, s)) →
  (∀ m . (MonadIO m, MonadReader r m, MonadState s m) ⇒ Event → m ()) →
  (∀ m . (MonadIO m, MonadReader r m, MonadState s m) ⇒ m ()) →
  IO s
window draw initialize processEvent tick = do

    True ← GLFW.init

    mapM GLFW.windowHint
      [ GLFW.WindowHint'Resizable True
      , GLFW.WindowHint'Visible True
      , GLFW.WindowHint'Decorated True
      , GLFW.WindowHint'RedBits 8
      , GLFW.WindowHint'GreenBits 8
      , GLFW.WindowHint'BlueBits 8
      , GLFW.WindowHint'DepthBits 1
      ]

    Just w <- createWindow 800 600 "slingspace" Nothing Nothing

    GLFW.makeContextCurrent (Just w)

    -- rr ← getWindowRefreshRate
    let rr = 60

    poll ← GLFWutil.prepareListPoll w

    True <- GLFW.extensionSupported "GLX_EXT_swap_control_tear"

    GLFW.swapInterval (-1)

    let
      loop = do
        get >>= draw
        liftIO $ GLFW.swapBuffers w
        liftIO poll >>= mapM processEvent
        b ← liftIO $ GLFW.windowShouldClose w
        when (not b) $ tick >> loop

    (r, ()) ← initialize rr >>= uncurry (execRWST loop)
    GLFW.destroyWindow w
    GLFW.terminate
    return r

