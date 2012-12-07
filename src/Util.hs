{-# LANGUAGE RecordWildCards, UnicodeSyntax, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ExistentialQuantification, ConstraintKinds #-}

module Util where

import Prelude hiding ((.))
import Data.List (genericLength)
import Control.Monad (liftM)
import System.IO (hFlush, stdout)
import System.Clock (getTime, Clock(Monotonic), TimeSpec, sec, nsec)
import Control.Monad.Random (MonadRandom, getRandomR)
import Language.Haskell.Interpreter (runInterpreter, loadModules, setImportsQ, interpret, infer)
import Data.Typeable (Typeable)

#ifdef linux_HOST_OS

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

read_config_file :: Read a ⇒ String → IO a
read_config_file f = liftM read $ readFile =<< getDataFileName ("config/" ++ f)

loadConfig :: Typeable a ⇒ String → IO a
loadConfig s = runInterpreter i >>= either (error . show) return
  where
    i = do
      loadModules [s]
      setImportsQ [("SlingSpace.Configuration", Nothing), ("Configuration", Nothing)]
      interpret "Configuration.config" infer

bounded :: Ord a ⇒ a → a → a → a
bounded n lower upper = min upper $ max lower $ n

doing :: String → IO a → IO a
doing s a = do
  putStr $ s ++ ".. "
  hFlush stdout
  x ← a
  putStrLn "done"
  return x


splitOnce :: String → Char → (String, String)
splitOnce "" _ = ("", "")
splitOnce (sh:st) c | sh == c = ("", st)
splitOnce (sh:st) c = let (x, y) = splitOnce st c in (sh:x, y)

timeSpecAsNanoSecs :: TimeSpec → Integer
timeSpecAsNanoSecs t = toInteger (sec t) * 1000000000 + toInteger (nsec t)

getMonotonicNanoSecs :: IO Integer
getMonotonicNanoSecs = timeSpecAsNanoSecs . getTime Monotonic

getMonotonicMilliSecs :: IO Integer
getMonotonicMilliSecs = (`div` 1000000) . getMonotonicNanoSecs

class TupleToList t a | t → a where tupleToList :: t → [a]
instance TupleToList (a, a) a where tupleToList (x, y) = [x, y]
instance TupleToList (a, a, a) a where tupleToList (x, y, z) = [x, y, z]

whenJust :: Monad m ⇒ Maybe a → (a → m ()) → m ()
whenJust = flip (maybe (return ()))

orElse :: Maybe a -> a -> a
orElse Nothing d = d
orElse (Just x) _ = x

randomItem :: (Functor m, MonadRandom m) => [a] → m a
randomItem xs = fmap (xs !!) $ getRandomR (0, length xs - 1)

average :: Fractional b => [b] -> b
average l = sum l / fromInteger (genericLength l)

data Any u = forall c . u c => Any c
  -- todo: there's probably some module which has this
