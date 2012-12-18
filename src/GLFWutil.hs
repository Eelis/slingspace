{-# LANGUAGE UnicodeSyntax, LambdaCase, ViewPatterns #-}

module GLFWutil (Event(..), prepareListPoll, getWindowRefreshRate) where

import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import System.Exit (ExitCode(ExitSuccess))
import qualified Data.Char as Char
import qualified System.Process as Process
import qualified Graphics.UI.GLFW as GLFW

-- The normal GLFW way of responding to events is by registering callbacks that do stuff. Most of these callbacks have an IO() result, meaning that in order to communicate with the rest of the program, they have to use things like IORef, which is ugly. Fortunately, we can implement a general way to communicate these events back to the rest of the program, namely by just recording them in a list and having the poll operation return the list. poll is typically called at the heart of the program loop, which is an excellent place to pass the events on for interpretation and state update, without any need for further IORefs.

data Event
  = WindowSizeEvent Int Int
  | WindowRefreshEvent
  | CharEvent Char Bool
  | KeyEvent GLFW.Key Bool
  | MouseButtonEvent GLFW.MouseButton Bool
  | MousePositionEvent Int Int
  | MouseWheelEvent Int

-- note: we don't do window close because it's a special callback because glfw interprets its return value

prepareListPoll :: IO (IO [Event])
prepareListPoll = do
  r ← newIORef []
  let push x = modifyIORef' r (x :)
  GLFW.setWindowRefreshCallback $ push WindowRefreshEvent
  GLFW.setCharCallback $ \c b → push $ CharEvent c b
  GLFW.setKeyCallback $ \k b → push $ KeyEvent k b
  GLFW.setMouseButtonCallback $ \m b → push $ MouseButtonEvent m b
  GLFW.setMousePositionCallback $ \x y → push $ MousePositionEvent x y
  GLFW.setMouseWheelCallback $ \i → push $ MouseWheelEvent i
  GLFW.setWindowSizeCallback $ \x y → push $ WindowSizeEvent x y
    -- beware: glfw "helpfully" calls the window size callback immediately when you set it, so we're already writing to r here
  return $ do
    GLFW.pollEvents
    GLFW.pollEvents
      -- two polls needed because glfw's cursor code is crap
    x ← readIORef r
    writeIORef r []
    return $ reverse x

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
    Nothing → GLFW.getWindowRefreshRate
