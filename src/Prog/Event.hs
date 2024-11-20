{-# LANGUAGE Strict #-}
-- | events are processed in the parent thread so
--   as little work as possible is done here
module Prog.Event where
-- events and exceptions are handled
import Prelude()
import UPrelude
import Control.Monad.State.Class (modify,gets)
import System.Exit (exitSuccess)
import qualified Vulk.GLFW as GLFW
import Prog
    ( MonadIO(liftIO), Prog, MonadReader(ask), MonadState(get) )
import Prog.Data
    ( Env(..),
      State(..) )
import Prog.Util ( logError, logExcept, logInfo, logWarn, logDebug )
import Prog.KeyEvent ( evalKey, updateInputState )
import Sign.Data ( Event(..), LogLevel(..), SysAction(..)
                 , InputEvent(..), TestParam(..) )
import Sign.Except ( ExType(ExVulk) )
import Sign.Queue ( tryReadQueue )
import Sign.Var ( atomically, modifyTVar' )

-- | reads event channel, then exectutes events recursively
processEvents ∷ Prog ε σ ()
processEvents = do
  env ← ask
  event ← liftIO $ atomically $ tryReadQueue $ envEventQ env
  case event of
    Just e → do
      processEvent e
      processEvents
    Nothing → return ()
-- | case statement on each event, these are mostly callbacks
--   since we want as little work as possible here
processEvent ∷ Event → Prog ε σ ()
processEvent event = case event of
  -- this bit only parses glfw errors
  (EventError err str) → do
    st ← get
    _  ← logExcept err ExVulk str
    case stWindow st of
      Just win → liftIO $ GLFW.setWindowShouldClose win True
      Nothing  → logError "no glfw window to close"
  -- multiple types of logging
  (EventLog (LogDebug _    ) str) → logDebug str
  (EventLog LogInfo          str) → logInfo  str
  (EventLog LogWarn          str) → logWarn  str
  (EventLog LogError         str) → logError str
  (EventLog _        str) → logInfo $ "unknown log type: " ⧺ str
  -- testing of system
  (EventSys (SysTest tp)) → testParam tp
  -- this will exit the game
  (EventSys SysExit) → do
    st ← get
    case stWindow st of
      Just win → liftIO $ GLFW.setWindowShouldClose win True
      Nothing  → liftIO exitSuccess
  -- recreation of the swapchain, reloading all textures
  -- processing of input occurs in the input thread
  (EventInput (InputKey         win k _ ks mk))
    → evalKey win k ks mk

-- | case statement to test system parameters
testParam ∷ TestParam → Prog ε σ ()
testParam TPWindow = do
  w ← gets stWindow
  printWindow w
testParam TPNULL   = logInfo "TPNULL"

printWindow ∷ Maybe GLFW.Window → Prog ε σ ()
printWindow Nothing  = logInfo "no window found"
printWindow (Just w) = do
  winPos  ← liftIO $ GLFW.getWindowPos  w
  winSize ← liftIO $ GLFW.getWindowSize w
  let printstring = "Window:\n  Pos:  " ⧺ (show winPos)
                  ⧺ "\n  Size: " ⧺ (show winSize)
                  ⧺ "\n"
  logInfo printstring
