module MyUtil
  ( (.), getDataFileName, simple_getOpt, minimumByMeasure, read_config_file
  , doing, timeofday_usecs, repeat_every, omni_map, htons, getlineSR, bounded, forever, spawn, sendAll, SockReader(..), withResource, withResource'
  ) where

import Prelude hiding (catch, (.))
import Data.IORef
import MyGL ()
import Data.List
import Graphics.UI.GLUT ()
import Network.Socket
import Control.Monad
import Control.Exception
import System.IO
import Foreign
import System (getArgs)
import GHC.Conc
import System.Console.GetOpt

#ifdef linux_HOST_OS

import System.Posix.Unistd(usleep)
import qualified Paths_slingspace

getDataFileName :: FilePath → IO String
getDataFileName = Paths_slingspace.getDataFileName

#else

import System.Win32.Time(getSystemTimeAsFileTime, FILETIME(..))

getDataFileName :: FilePath → IO String
getDataFileName = return -- On Windows we just want a simple in-place runnable thing.

#endif

(.) :: Functor f ⇒ (a → b) → f a → f b
(.) = fmap

simple_getOpt :: [OptDescr f] → s → (f → s → s) → IO s
  -- Uses getArgs, Permute, accepts flags only (no additional arguments), reports errors with "error", and has a fixed header, later options override earlier options. In all, quite a big loss of genericity, but it suits my needs.
simple_getOpt opts s c = do
  args ← getArgs
  case getOpt Permute opts args of
    (flags, [], []) → return $ foldr c s flags
    (_, _, errs) → error $ concat errs ++ usageInfo "Parameters:" opts

read_config_file :: Read a ⇒ String → IO a
read_config_file f = liftM read $ readFile =<< getDataFileName ("config/" ++ f)

class IOResource a where dealloc :: a → IO ()

withResource :: IOResource a ⇒ IO a → (a → IO b) → IO b
withResource x = bracket x dealloc

withResource' :: IOResource a ⇒ a → IO b → IO b
withResource' x y = bracket (return x) dealloc $ const y

instance IOResource Socket where dealloc = sClose

bounded :: Ord a ⇒ a → a → a → a
bounded n lower upper = min upper $ max lower $ n

doing :: String → IO a → IO a
doing s a = do
  putStr $ s ++ ".. "
  hFlush stdout
  x ← a
  putStrLn "done"
  return x

spawn :: IO () → IO ThreadId
spawn act = do
  t ← myThreadId
  forkIO $ act -- `catch` throwTo t

atomicModifyTVar :: TVar a → (a → a) → IO ()
atomicModifyTVar v f = atomically $ readTVar v >>= writeTVar v . f

tryConditionallyModifyTVar :: TVar a → (a → Bool) → (a → a) → IO Bool
tryConditionallyModifyTVar v c f =
  atomically $ do
    x ← readTVar v
    if c x
     then writeTVar v (f x) >> return True
     else return False

sendAll :: Socket → String → IO ()
sendAll _ "" = return ()
sendAll sock str = do { sent ← send sock str; sendAll sock $ drop sent str }

data SockReader = SockReader Socket (IORef String)

splitOnce :: String → Char → (String, String)
splitOnce "" _ = ("", "")
splitOnce (sh:st) c | sh == c = ("", st)
splitOnce (sh:st) c = let (x, y) = splitOnce st c in (sh:x, y)

getlineSR :: SockReader → IO String
getlineSR sr@(SockReader s br) = do
  b ← readIORef br
  let (x, y) = splitOnce b '\n'
  if length x == length b
   then do
    m ← recv s 50000
    writeIORef br (b ++ m)
    getlineSR sr
   else writeIORef br y >> return x

foreign import ccall unsafe "htons" htons :: Word16 → Word16

#ifdef linux_HOST_OS

foreign import ccall "cutil.h timeofday_usecs" timeofday_usecs :: IO (Word64)

repeat_every :: Word64 → IO () → IO ()
repeat_every usecs a = do
  timeofday_usecs >>= f
  where
   f l = do
    a
    n ← timeofday_usecs
    when (l + usecs > n) $ do
      let s = (l + usecs - n)
      usleep (fromInteger $ toInteger $ s)
    f (l + usecs)

#else

timeofday_usecs = do
  FILETIME t ← getSystemTimeAsFileTime
  return $ t `quot` 10

#endif

minimumByMeasure :: Ord b ⇒ (a → b) → [a] → a
minimumByMeasure f l = snd $ minimumBy (\x y → compare (fst x) (fst y)) $ map (\x → (f x, x)) l

omni_map :: (a → [b] → b) → [a] → [b]
omni_map _ [] = []
omni_map f (h:t) = f h t' : t' where t' = omni_map f t
