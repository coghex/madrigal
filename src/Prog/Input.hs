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
import Prog.KeyEvent ( changeKeyMap, findKey, lookupKey, indexKeyMap
                     , stateKeyPress, stateKeyRelease )
import Prog.Init ( initKeyMap, initInpState )
import Prog.Util ()
import Sign.Data
    ( Event(..), LogLevel(..),
      SysAction(..), TState(..)
    , SettingsChange(..), InputStateChange(..) )
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
runInputLoop env win inpSt keyMap TStop = do
  let timerChan = envInpCh env
  tsNew ← atomically $ readChan timerChan
  runInputLoop env win inpSt keyMap tsNew
runInputLoop env win inpSt0 keyMap0 TStart = do
  start ← getCurrentTime
  let timerChan = envInpCh env
  timerState ← atomically $ tryReadChan timerChan
  tsNew ← case timerState of
            Nothing → return TStart
            Just x  → return x
  (inpSt2,keyMap1) ← processLoadInputs env win inpSt0 keyMap0
  inpSt3 ← processInputSideEffects env inpSt2
  end ← getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = 1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  runInputLoop env win inpSt3 keyMap1 tsNew
-- pause not needed for this timer
runInputLoop _ _ _ _ TPause = return ()
runInputLoop _ _ _ _ TNULL  = return ()

-- | this will recursively process a queue of inputs, allowing
--   each processing to return a result type, and then preforming
--   actions on that type
processLoadInputs ∷ Env → GLFW.Window → InputState → KeyMap
  → IO (InputState,KeyMap)
processLoadInputs env win inpSt keymap = do
  rawInp ← atomically $ tryReadQueue $ envInpQ env
  case rawInp of
    Just inp → do
      ret ← processLoadInput env win inpSt keymap inp
      case ret of
        -- if input is successful keep processing incoming input
        ResInpSuccess   → processLoadInputs env win inpSt keymap
        -- if we need to change input state
        ResInpState is' → case inpStatus is' of
          ISSNULL → processLoadInputs env win is' keymap
          ISSLogDebug str → do
            let eventQ = envEventQ env
            atomically $ writeQueue eventQ $ EventLog (LogDebug 1) str
            processLoadInputs env win is'' keymap
              where is'' = is' { inpStatus = ISSNULL }
        -- for the ingame changing of the keymap
        ResInpChangeKey keyFunc key 1 → do
--          atomically $ writeQueue (envEventQ env)
--            $ EventLog (LogDebug 1) $ "new keys: " ⧺ show keymap'
          processLoadInputs env win inpSt' keymap'
            where keymap' = changeKeyMap keyFunc key 1 keymap
                  inpSt'  = inpSt { inpCap = CapKeyChange 2 keyFunc }
                  secondkey = last $ indexKeyMap keymap' keyFunc
        ResInpChangeKey keyFunc key 2 → do
--          atomically $ writeQueue (envEventQ env)
--            $ EventLog (LogDebug 1) $ "new keys: " ⧺ show keymap'
          atomically $ writeQueue (envEventQ env)
            $ EventSettings $ SettingsChangeKeyMap keymap'
          processLoadInputs env win inpSt' keymap'
            where keymap' = changeKeyMap keyFunc key 2 keymap
                  inpSt'  = inpSt { inpCap = CapNULL }
        ResInpChangeKey keyFunc key n → do
        -- code should never reach here
          processLoadInputs env win inpSt keymap'
            where keymap' = changeKeyMap keyFunc key n keymap
        -- if we run across an error, we can return it here, print
        -- to the main thread and then continue to process input.
        -- the main event thread is responsible to check if we can
        -- continue to program or not
        ResInpError str    → do
          atomically $ writeQueue (envEventQ env)
            $ EventLog (LogDebug 1) $ "input error: " ⧺ str
          processLoadInputs env win inpSt keymap
        -- null result will stop all processing for that frame,
        -- they actions will remain in the queue and should be
        -- processed next tick
        ResInpNULL → do
          atomically $ writeQueue (envEventQ env)
            $ EventLog (LogDebug 1) "input null command"
          return (inpSt,keymap)
    Nothing  → return (inpSt,keymap)

-- | indiviual input command processing
processLoadInput ∷ Env → GLFW.Window → InputState → KeyMap
  → InputAct → IO InpResult
processLoadInput env win inpSt keymap inp = case inp of
  -- key press action, if the keys have been captured by a
  -- window, we need to function differently
  InpActKey k ks _  → case inpCap inpSt of
    -- input captured by key change pop up window
    CapKeyChange n keyFunc → if ks ≡ GLFW.KeyState'Pressed then do
        --print $ "captured: " ⧺ show k ⧺ " for key func: " ⧺ show keyFunc
        -- test keys still work for now
        if lookupKey keymap (findKey k) ≡ KFTest
          then do
            return ResInpSuccess
        else if lookupKey keymap (findKey k) ≡ KFTest2
          then do
            return ResInpSuccess
        else return $ ResInpChangeKey keyFunc (findKey k) n
      else return ResInpSuccess
    -- for when text is being input
    CapTextInput str → if ks ≡ GLFW.KeyState'Pressed then do
        --print $ "captured: " ⧺ show k ⧺ " for text input"
        if lookupKey keymap (findKey k) ≡ KFReturn
          then do
            return $ ResInpState inpSt { inpCap = CapNULL }
          else do
            sc ← GLFW.getKeyScancode k
            k' ← GLFW.getKeyName k sc
            let newstr = str ⧺ fromMaybe [] k'
            return $ ResInpState inpSt { inpCap = CapTextInput newstr }
      else return ResInpSuccess
    -- if no input capture, case on each key function, set global inp state through event
    CapNULL → if ks ≡ GLFW.KeyState'Pressed then case lookupKey keymap (findKey k) of
        KFEscape → do
          atomically $ writeQueue (envEventQ env) $ EventSys SysExit
          return ResInpSuccess
        KFFullScreen → do
          return ResInpSuccess
        KFTest → do
          return ResInpSuccess
        KFTest2 → do
          return ResInpSuccess
        KFUnknown str → do
          atomically $ writeQueue (envEventQ env) $ EventLog LogWarn $ "key " ⧺ str ⧺ " not set in key map"
          return ResInpSuccess
        keyFunc → do
          --atomically $ writeQueue (envEventQ env)
          --  $ EventLog (LogDebug 1) $ "unknown key " ⧺ show keyFunc
          -- update global input state
          atomically $ writeQueue (envEventQ env)
            $ EventInputState $ ISCKeyPress keyFunc
          let newis = stateKeyPress keyFunc inpSt
          return $ ResInpState newis
      else if ks ≡ GLFW.KeyState'Released then case lookupKey keymap (findKey k) of
        keyFunc → do
          atomically $ writeQueue (envEventQ env)
            $ EventInputState $ ISCKeyRelease keyFunc
          let newis = stateKeyRelease keyFunc inpSt
          return $ ResInpState newis
      else return ResInpSuccess
  InpActResetCam → do
    -- make sure to also clear the copy of this data in the main thread
    atomically $ writeQueue (envEventQ env) $ EventInputState ISCResetCam
    return $ ResInpState $ inpSt { keySt = (keySt inpSt) { keyAccel = (0,0) } }
  InpActTest → do
    atomically $ writeQueue (envEventQ env) $ EventLog LogInfo $ show inpSt
    return ResInpSuccess
  InpActNULL             → return ResInpSuccess

-- execute side effects of input state, despite its name it atually creates
-- non-side effects
processInputSideEffects ∷ Env → InputState → IO (InputState)
processInputSideEffects env is = return is
