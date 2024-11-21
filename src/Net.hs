-- | a thread that handles socket connections
module Net where

import Prelude()
import UPrelude
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Exception
import Control.Monad ( when )
import Control.Monad.Fix ( fix )
import Net.Data ( NetServiceType(..), NetAction(..)
                , NetState(..), NetStatus(..), NetTestParam(..)
                , NetService(..), NetResult(..) )
import Prog ( MonadIO(liftIO), MonadReader(ask) )
import Prog.Data ( Env(..) )
import Sign.Data ( Event(..), LogLevel(..), TState(..), SysAction(..) )
import Sign.Var ( atomically, readTVar, writeTVar, modifyTVar' )
import qualified Sign.Queue as SQ ( writeQueue, tryReadQueue, readChan, tryReadChan )
import Control.Concurrent (threadDelay)
import Data.Time.Clock ( diffUTCTime, getCurrentTime )

networkThread ∷ Env → IO ()
networkThread env = do
  atomically $ SQ.writeQueue (envEventQ env)
    $ EventLog (LogDebug 2) "starting network thread..."
  runNetLoop env (NetState []) TStop

runNetLoop ∷ Env → NetState → TState → IO ()
runNetLoop env netState TStop = do
  let timerChan = envNetCh env
  tsNew ← atomically $ SQ.readChan timerChan
  runNetLoop env netState tsNew
runNetLoop env netState0 TStart = do
  start ← getCurrentTime
  let timerChan = envNetCh env
  timerState ← atomically $ SQ.tryReadChan timerChan
  tsNew ← case timerState of
            Nothing → return TStart
            Just x  → return x
  netState1  ← processNets env netState0
  netState2 ← startNets env netState1
  end ← getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = 1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  runNetLoop env netState2 tsNew
runNetLoop _ _ TPause = return ()
runNetLoop _ _ TNULL  = return ()

-- | executes commands in the net queue
processNets ∷ Env → NetState → IO (NetState)
processNets env netstate = do
  rawInp ← atomically $ SQ.tryReadQueue $ envNetQ env
  case rawInp of
    Just inp → do
      ret ← processNet env netstate inp
      case ret of
        ResNetSuccess   → processNets env netstate
        ResNetError str → do
          atomically $ SQ.writeQueue (envEventQ env)
            $ EventLog (LogDebug 1) $ "input error: " ⧺ str
          processNets env netstate
        ResNetNewService ns → do
          return $ netstate { netStateServices = oldnss ⧺ [ns] }
            where oldnss = netStateServices netstate
        ResNetNULL          → do
          atomically $ SQ.writeQueue (envEventQ env)
            $ EventLog (LogDebug 1) "input null command"
          return (netstate)
    Nothing → return netstate

-- | executes a single net command
processNet ∷ Env → NetState → NetAction → IO (NetResult)
processNet env netstate netaction = case netaction of
  NetActionNewService NSCommand → do
    return $ ResNetNewService ns
      where ns = NetService { nsType   = NSCommand
                            , nsStatus = NetStatusStarting
                            , nsPort   = port }
            port = findSuitablePort (netStateServices netstate)
  NetActionTest NetTestPrintNet → do
    let networkstring = show netstate
    atomically $ SQ.writeQueue (envEventQ env) $ EventLog LogInfo networkstring
    return ResNetSuccess
  NetActionTest NetTestNULL     → return ResNetSuccess
  NetActionNewService NSNULL    → return ResNetSuccess
  NetActionNULL                 → return ResNetSuccess

-- | increments the port for every new service
--   TODO: it should reuse unused old ports
findSuitablePort ∷ [NetService] → Int
findSuitablePort []  = 1337
findSuitablePort nss = nsPort (last nss) + 1

-- | starts any unstarted net services
startNets ∷ Env → NetState → IO NetState
startNets env (NetState netservices) = do
  nss ← startNets' env netservices
  return $ NetState nss
startNets' ∷ Env → [NetService] → IO [NetService]
startNets' _   []     = return []
startNets' env (n:ns) = do
  nss ← startNets' env ns
  n'  ← startNet   env n
  return $ n' : nss
startNet ∷ Env → NetService → IO NetService
startNet env ns = case (nsStatus ns) of
  NetStatusStarting → do
    res ← startNetThread env (nsType ns) (nsPort ns)
    let ns' = ns { nsStatus = NetStatusRunning }
    return ns'
  NetStatusRunning  → return ns
  NetStatusNULL     → return ns
startNetThread ∷ Env → NetServiceType → Int → IO NetResult
startNetThread env NSCommand port = do
  _ ← forkIO $ netThread env NSCommand port
  return ResNetSuccess
startNetThread _   NSNULL    _    = return ResNetSuccess
netThread ∷ Env → NetServiceType → Int → IO ()
netThread env NSCommand port = do
  startCommandThread env $ fromIntegral port
netThread _   NSNULL    _    = return ()

startCommandThread ∷ Env → PortNumber → IO ()
startCommandThread env port = do
  sock ← socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port 0)
  listen sock 2
  chan ← newChan
  _ ← forkIO $ fix $ \loop → do
    (_, _) ← readChan chan
    loop
  commandloop env sock chan 0

type Msg = (Int,String)

commandloop ∷ Env → Socket → Chan Msg → Int → IO ()
commandloop env sock chan msgNum = do
  conn ← accept sock
  forkIO (runCommand env conn chan msgNum)
  commandloop env sock chan $! msgNum + 1

runCommand ∷ Env → (Socket, SockAddr) → Chan Msg → Int → IO ()
runCommand env (sock, _) chan msgNum = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl ← socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    commLine ← dupChan chan
    reader ← forkIO $ fix $ \loop → do
      hPutStr hdl $ "(madrigal) $> "
      (nextNum, line) ← readChan commLine
      loop
    handle (\(SomeException _) → return ()) $ fix $ \loop → do
      line ← fmap init (hGetLine hdl)
      processInput env hdl line
      case line of
        "exit" → hPutStrLn hdl "exiting..."
        _      → broadcast ("> " ⧺ line) >> loop
    killThread reader
    broadcast "goodbye"
    hClose hdl

processInput ∷ Env → Handle → String → IO ()
processInput env hdl "quit" = do
  hPutStrLn hdl "quitting..."
  atomically $ SQ.writeQueue (envEventQ env) $ EventSys SysExit
processInput _   hdl inp
  = hPutStrLn hdl $ "unknown command: " ⧺ inp
