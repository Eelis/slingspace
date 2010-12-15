
import Prelude hiding (catch, (.))
import Gui
import Data.IORef
import Control.Monad
import Logic
import MyGL ()
import Graphics.UI.GLUT (Vector3(..))
import System.IO
import MyUtil
import Network.Socket
import Network.BSD
import Data.Maybe
import Math
import Control.Exception
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import System.Console.GetOpt
import Foreign
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

#ifdef linux_HOST_OS
import System.Posix.User
#else
getLoginName = return "WindowsUser" -- extdep: System.Win32 doesn't seem to provide GetUserName
#endif

data RemoteGuiCallback = RGC
  { rgc_chan :: TChan ClientToServerMsg
  , rgc_cfg :: GameplayConfig
  , rgc_pls :: IORef (Map String (V, Player))
  }

instance GuiCallback RemoteGuiCallback where
  cc_tick r = modifyIORef (rgc_pls r) $ (.) (\(corr, p) -> (corr <*> 0.96, tick_player (rgc_cfg r) p))
  cc_release r g = atomically $ writeTChan (rgc_chan r) $ Release g
  cc_fire r g v = atomically $ writeTChan (rgc_chan r) $ FireAt g v
  cc_spawn r = atomically $ writeTChan (rgc_chan r) $ Spawn
  cc_players = ((uncurry move .) .) . readIORef . rgc_pls

data Flag = NameFlag String | PortFlag Word16 | HostFlag String

data PlayerNameConfig = SystemUserName | CustomName String deriving Read

data ClientConfig = ClientConfig
  { player_name :: PlayerNameConfig
  , default_host :: String
  , default_port :: Word16
  } deriving Read

main :: IO ()
main = flip catch print $ do

  client_cfg <- read_config_file "client.txt"

  tentative_player_name <-
    case player_name client_cfg of
      SystemUserName -> getLoginName
      CustomName n -> return n

  let
    options =
      [ Option "n" ["name"] (ReqArg NameFlag "<token>") $
          "player name (defaults to " ++ tentative_player_name ++ ")"
      , Option "p" ["port"] (ReqArg (PortFlag . read) "<number>") $
          "server port (defaults to " ++ show (default_port client_cfg) ++ ")"
      , Option "h" ["host"] (ReqArg HostFlag "<addr>") $
          "server address (defaults to " ++ default_host client_cfg ++ ")"
      ]

  (plname, host_name, host_port) <- simple_getOpt options
    (tentative_player_name, default_host client_cfg, default_port client_cfg) $
      (\f (pn, hn, hp) ->
        case f of
          NameFlag n -> (n, hn, hp)
          PortFlag p -> (pn, hn, p)
          HostFlag n -> (pn, n, hp))

  host_address:_ <- hostAddresses . getHostByName host_name

  withResource (socket AF_INET Stream 6) $ \sock -> do

  doing "connecting" $ connect sock $ SockAddrInet (PortNum $ htons host_port) host_address

  sendAll sock $ plname ++ "\n"

  sr <- SockReader sock . newIORef ""
  greeting <- doing "receiving initial data" $ read . getlineSR sr

  case greeting of
    PissOff reason -> putStrLn $ "Not allowed to join because: " ++ reason
    Welcome cfg obs -> do
      let graphnode_map = to_graphnode_map $ from_network_obs obs

      doing "pre-processing obstacles" $ print $ sum $ length . gn_neighbours . Map.elems graphnode_map

      let closest = head $ Map.elems graphnode_map
        -- bad guess, but will be corrected in few ticks
      chan <- newTChanIO
      spawn $ forever $ sendAll sock . (++ "\n") . show =<< atomically (readTChan chan)
      players <- newIORef (Map.empty :: Map String (V, Player))
      spawn $ forever $ do
        msg <- read . getlineSR sr
        case msg of
          TextMsg text -> putStrLn text
          Teleport who (SerializablePlayer x y z) ->
            modifyIORef players $ Map.insert who (Vector3 0 0 0, Player x y z closest)
          Players new_pls ->
            modifyIORef players $ \old_pls -> flip Map.mapWithKey new_pls $ \n sp@(SerializablePlayer x y z) ->
              case Map.lookup n old_pls of
                Nothing -> (Vector3 0 0 0, Player x y z closest)
                Just (old_c, old_p) -> (pb_pos (body old_p) <+> old_c <-> pb_pos x, update_player old_p sp)
            --writeIORef players (fmap (\x -> (Vector3 0 0 0, x)) new_pls)
      gui (RGC chan cfg players) plname cfg
