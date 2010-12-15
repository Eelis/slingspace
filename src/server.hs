
import Logic
import Prelude hiding (catch, (.))
import Data.IORef
import GHC.Conc
import StaticConfig
import MyGL ()
import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.UI.GLUT
import Network.Socket
import Control.Monad
import System.IO
import System.Random
import Math
import Data.Word
import Obstacles
import MyUtil
import Control.Exception
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import System.Console.GetOpt

fresh_player :: GraphNode -> Bool -> Player
fresh_player closest d = Player (PlayerBody (Vector3 0 1800 1000) (Vector3 0 0 0)) Map.empty d closest

sendMsg :: Show a => a -> TChan String -> IO ()
sendMsg m c = atomically $ writeTChan c $ show m ++ "\n"

data ServerConfig = ServerConfig
  { default_port :: Word16
  , playerdata_broadcast_interval :: Integer
  } deriving (Read)

data Flag = PortFlag Word16

data ConnectedPlayer = CPlayer
  { cp_tchan :: TChan String
  , cp_player :: Player
  }

main :: IO ()
main = withSocketsDo $ do

  server_cfg <- read_config_file "server.txt"
  gameplay_cfg <- read_config_file "gameplay.txt"

  let { options = [ Option "p" ["port"] (ReqArg (PortFlag . read) "<number>") $
        "port (defaults to " ++ show (default_port server_cfg) ++ ")" ] }

  port <- simple_getOpt options (default_port server_cfg) $ \_ p -> p
  obsts <- fst . readRngMonad bigField . getStdGen

  let graphnode_map = to_graphnode_map obsts
  forM (Map.elems graphnode_map) $ print . length . gn_neighbours

  let closest_guess = head $ Map.elems graphnode_map

  print $ length $ show obsts -- forces evaluation or something. without this, total breakage. i tried $! above, but it had no effect

  withResource (socket AF_INET Stream 6) $ \accept_sock -> do

  bindSocket accept_sock $ SockAddrInet (PortNum $ htons $ port) iNADDR_ANY
  listen accept_sock 5

  players <- newMVar (Map.empty :: Map String ConnectedPlayer)
    -- because otherwise tick-broadcasts and join-broadcasts can interleave

  putStrLn "accepting connections"

  spawn $ forever $ do
    (sock, remote_addr) <- accept accept_sock
    putStrLn $ "Incoming connection from " ++ show remote_addr
    forkIO $ withResource' sock $ clientfunc closest_guess players gameplay_cfg obsts sock `catch` print

  ir <- newIORef (0 :: Integer)

  repeat_every (fromInteger $ tick_duration * 1000) $ do
    p <- modifyMVar players $ \p ->
      return ((\cp -> cp { cp_player = tick_player gameplay_cfg (cp_player cp) }) . p, p)
    i <- readIORef ir
    if i /= playerdata_broadcast_interval server_cfg then writeIORef ir (i + 1) else do
      writeIORef ir 0
      broadcast_playerdata p

broadcast_playerdata :: Map String ConnectedPlayer -> IO ()
broadcast_playerdata p =
  forM_ (Map.elems p) $ sendMsg (Players $ serialize_player . cp_player . p) . cp_tchan

clientfunc :: GraphNode -> MVar (Map String ConnectedPlayer) -> GameplayConfig -> [AnnotatedObstacle] -> Socket -> IO ()
clientfunc closest_guess players cfg obsts sock = do
  sr <- SockReader sock . newIORef ""
  name <- getlineSR sr

  let spawnP c = CPlayer c (fresh_player closest_guess False)

  r <- modifyMVar players $ \p -> if Map.member name p then return (p, Nothing) else do
    chan <- newTChanIO
    sendMsg (Welcome cfg $ NO . (obs_vertices .) . ao_triangles . obsts) chan
      -- needs to be done here because as soon as we commit players, the main thread can enqueue other messages, and we need the welcome message to be first
    return (Map.insert name (spawnP chan) p, Just (p, chan))

  case r of
    Nothing -> do
      sendAll sock $ show (PissOff "name taken") ++ "\n"
      putStrLn $ "rejected " ++ name ++ " (name taken)"
    Just (p, chan) -> do
      putStrLn $ name ++ " joined"

      forM_ (cp_tchan . Map.elems p) $ atomically . flip writeTChan (
        show (TextMsg $ name ++ " joined") ++ "\n" ++
        show (Players $ serialize_player . cp_player . p) ++ "\n")

      flip finally (putStrLn $ name ++ " left") $ do

      flip finally (modifyMVar_ players $ return . Map.delete name) $ do

      spawn $ forever $ sendAll sock =<< atomically (readTChan chan) -- process outgoing messages

      forever $ do -- process incoming messages
        msg <- read . getlineSR sr
        modifyMVar_ players $ \q ->
          case msg of
            FireAt g t -> do
              let p' = Map.adjust (\cp -> cp { cp_player = fire cfg g t (cp_player cp)}) name q
              broadcast_playerdata p'
              return p'
            Release g -> do
              let p' = Map.adjust (\cp -> cp { cp_player = release g (cp_player cp) }) name q
              broadcast_playerdata p'
              return p'
            Spawn -> do
              let p' = Map.insert name (spawnP chan) q
              forM_ (Map.elems p') $ sendMsg (Teleport name $ serialize_player $ cp_player $ spawnP chan) . cp_tchan
              return p'
