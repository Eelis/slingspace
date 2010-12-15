
import Logic
import Prelude hiding (catch)
import Data.IORef
import GHC.Conc
import StaticConfig
import MyGL ()
import qualified Data.Map as Map
import Graphics.UI.GLUT
import System (getArgs)
import Network.Socket
import Control.Monad
import System.IO
import System.Random
import Math
import MyUtil
import Control.Exception
import Control.Concurrent.STM.TChan

fresh_player d = Player (PlayerBody (Vector3 0 1800 1000) (Vector3 0 0 0)) Map.empty d

main = withSocketsDo $ do
  [addr, port] ← getArgs

  cfg ← liftM read $ readFile "gameplay-config.txt"

  obsts ← liftM (concat . map (\(Obstacle a b c d) → [a, b, c, d]) . fst . readRngMonad (niceTunnel cfg)) getStdGen

  bracket (socket AF_INET Stream 6) sClose $ \accept_sock → do
  host_addr ← inet_addr addr

  bindSocket accept_sock $ SockAddrInet (PortNum $ htons $ read port) host_addr
  listen accept_sock 10

  players ← newTVarIO (Map.empty :: Map.Map String (TChan ServerToClientMsg, Player))

  putStrLn "accepting connections"

  spawn $ forever $ do
    (sock, remote_addr) ← accept accept_sock
    putStrLn $ "incoming connection from " ++ show remote_addr
    forkIO $ clientfunc players cfg obsts sock `finally` sClose sock `catch` print

  ir ← newIORef (0 :: Integer)

  repeat_every (fromInteger $ tick_duration * 1000) $ do
    i ← readIORef ir
    b ← if i == 100 then writeIORef ir 0 >> return True else writeIORef ir (i + 1) >> return False
    atomically $ do
      p ← readTVar players
      let p' = fmap (\( x, y) → (x, tick_player cfg (Just obsts) y)) p
      writeTVar players p'
      when b $ forM_ (Map.elems p') $ \(chan, _) → writeTChan chan $ Players $ fmap snd p'


clientfunc players cfg obsts sock = do
  sr ← liftM (SockReader sock) (newIORef "")
  name ← getlineSR sr
  chan ← newTChanIO

  success ← tryConditionallyModifyTVar players (not . Map.member name) $
    Map.insert name (chan, fresh_player True)
      -- TODO: no good. at this point the other thread can start sending playerdata before the greeting is sent.

  when (not success) $ do
    sendAll sock $ show (PissOff "name taken") ++ "\n"
    error $ "rejected " ++ name ++ " (name taken)"

  sendAll sock $ show (Welcome cfg obsts) ++ "\n"
  --broadcast $ TextMsg $ name ++ " joined\n"

  flip finally (putStrLn $ name ++ " left") $ do

  flip finally (atomicModifyTVar players $ Map.delete name) $ do

  spawn $ forever $ sendAll sock . (++ "\n") . show =<< atomically (readTChan chan)

  forever $ liftM read (getlineSR sr) >>= \msg → atomically $ do
    p ← readTVar players
    let
     p' = Map.adjust (\(x, y) →
      (x, case msg of
        FireAt g t → fire cfg g t y
        Spawn → fresh_player False
        Release g → release g y)) name p
    writeTVar players p'
    forM_ (Map.elems p') $ \(ch, _) → writeTChan ch $ Players $ fmap snd p'
