{-# LANGUAGE Strict #-}
-- | all input events are, in their callbacks, sending this thread
--   the input in the input queue.  it is then processed here
module Prog.Input where
-- a thread to handle input
import Prelude()
import UPrelude
import Data ( KeyFunc(..), KeyMap(..), PopupType(..), Cardinal(..) )
import Data.Maybe ( fromMaybe )
import Prog.Data
    ( Env(..), ISKeys(..), InpResult(..),
      ISStatus(ISSNULL, ISSLogDebug),
      InputState(..), InputAct(..) )
import Prog.KeyEvent ( changeKeyMap, findKey, lookupKey, indexKeyMap
                     , stateKeyPress, stateKeyRelease )
import Prog.Init ( initInpState, initKeyMap )
import Sign.Data
    ( Event(..), LogLevel(..),
      SysAction(..), TState(..)
    , InputStateChange(..), TestParam(..) )
import Sign.Var ( atomically, readTVar, writeTVar, modifyTVar' )
import Sign.Queue
    ( readChan, tryReadChan, tryReadQueue, writeQueue )
import Control.Concurrent (threadDelay)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Vulk.GLFW as GLFW

-- | threaded recursive loop
inputThread ∷ Env → GLFW.Window → IO ()
inputThread env win = do
  atomically $ writeQueue (envEventQ env)
    $ EventLog (LogDebug 2) "starting input thread..."
  runInputLoop env win initIS initKeyMap TStop
  where initIS = initInpState

-- | generic timed loop, so that the CPU can idle
runInputLoop ∷ Env → GLFW.Window → InputState → KeyMap → TState → IO ()
runInputLoop env win inpSt keymap TStop = do
  let timerChan = envInpCh env
  tsNew ← atomically $ readChan timerChan
  runInputLoop env win inpSt keymap tsNew
runInputLoop env win inpSt0 keymap0 TStart = do
  start ← getCurrentTime
  let timerChan = envInpCh env
  timerState ← atomically $ tryReadChan timerChan
  tsNew ← case timerState of
            Nothing → return TStart
            Just x  → return x
  _ ← processInputs env win inpSt0 keymap0
  end ← getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = 1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  runInputLoop env win inpSt0 keymap0 tsNew
-- pause not needed for this timer
runInputLoop _ _ _ _ TPause = return ()
runInputLoop _ _ _ _ TNULL  = return ()

-- | this will recursively process a queue of inputs, allowing
--   each processing to return a result type, and then preforming
--   actions on that type
processInputs ∷ Env → GLFW.Window → InputState → KeyMap
  → IO (InputState,KeyMap)
processInputs env win inpSt keymap = do
  rawInp ← atomically $ tryReadQueue $ envInpQ env
  case rawInp of
    Just inp → do
      ret ← processInput env win inpSt keymap inp
      case ret of
        ResInpSuccess   → processInputs env win inpSt keymap
        ResInpError str → do
          atomically $ writeQueue (envEventQ env)
            $ EventLog (LogDebug 1) $ "input error: " ⧺ str
          processInputs env win inpSt keymap
        ResInpNULL      → do
          atomically $ writeQueue (envEventQ env)
            $ EventLog (LogDebug 1) "input null command"
          return (inpSt, keymap)
    Nothing → return (inpSt, keymap)

-- | indiviual input command processing
processInput ∷ Env → GLFW.Window → InputState → KeyMap
  → InputAct → IO InpResult
processInput env win inpSt keymap inp = case inp of
  InpActKey k ks _ →
    if ks ≡ GLFW.KeyState'Pressed then case lookupKey keymap (findKey k) of
      KFEscape → do
        atomically $ writeQueue (envEventQ env) $ EventSys SysExit
        return ResInpSuccess
      KFTest → do
        atomically $ writeQueue (envEventQ env)
          $ EventSys $ SysTest $ TPNetwork
        return ResInpSuccess
      _        → return ResInpSuccess
    else return ResInpSuccess
  InpActNULL → return ResInpSuccess
