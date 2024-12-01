module Lua where
import Prelude()
import UPrelude
import Control.Concurrent ( threadDelay )
import Data.Time.Clock ( diffUTCTime, getCurrentTime )
import Lua.Data ( LuaResult(..), LuaAction(..) )
import Net.Data ( NetAction(..) )
import Prog.Data ( Env(..) )
import Sign.Data ( LogLevel(..), TState(..), Event(..) )
import Sign.Var ( atomically, readTVar, writeTVar, modifyTVar' )
import qualified Sign.Queue as SQ ( writeQueue, tryReadQueue, readChan, tryReadChan )
import qualified HsLua as L
import Data.ByteString.Char8 as BS

luaThread ∷ Env → IO ()
luaThread env = do
  atomically $ SQ.writeQueue (envEventQ env)
    $ EventLog (LogDebug 2) "starting lua thread..."
  let ls = envLuaSt env
  _ ← L.runWith ls $ do
    L.openlibs
  luaLoop env TStart
luaLoop ∷ Env → TState → IO ()
luaLoop env TStop = do
  let timerChan = envLuaCh env
  tsNew ← atomically $ SQ.readChan timerChan
  luaLoop env tsNew
luaLoop env TStart = do
  start ← getCurrentTime
  let timerChan = envLuaCh env
  timerState ← atomically $ SQ.tryReadChan timerChan
  tsNew ← case timerState of
            Nothing → return TStart
            Just x  → return x
  processLuaCmds env
  end ← getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = 1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  luaLoop env tsNew
luaLoop _ TPause = return ()
luaLoop _ TNULL  = return ()

processLuaCmds ∷ Env → IO ()
processLuaCmds env = do
  rawInp ← atomically $ SQ.tryReadQueue $ envLuaQ env
  case rawInp of
    Just inp → do
      ret ← processLuaCmd env inp
      case ret of
        ResLuaSuccess   → processLuaCmds env
        ResLuaError str → do
          atomically $ SQ.writeQueue (envEventQ env)
            $ EventLog (LogDebug 1) $ "input error: " ⧺ str
          processLuaCmds env
        ResLuaNULL      → atomically $ SQ.writeQueue (envEventQ env)
                            $ EventLog (LogDebug 1) "input null command"
    Nothing → return ()

processLuaCmd ∷ Env → LuaAction → IO (LuaResult)
processLuaCmd env (LuaActionCmdString hdl str) = do
  res ← L.runWith (envLuaSt env) $ do
    L.openlibs
    L.dostring $ BS.pack str
  atomically $ SQ.writeQueue (envNetQ env) $ NetActionOutp hdl $ show res
  return ResLuaSuccess
processLuaCmd env LuaActionNULL            = return $ ResLuaNULL
