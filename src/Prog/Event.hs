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
    ( Env(envEventQ),
      ReloadState(RSRecreate, RSReload),
      State(stWindow, stReload) )
import Prog.Util ( logError, logExcept, logInfo, logWarn, logDebug )
import Prog.KeyEvent ( evalKey, updateInputState )
import Prog.Mouse ( evalMouse, evalScroll )
import Sign.Data
import Sign.Except ( ExType(ExVulk) )
import Sign.Queue ( tryReadQueue )
import Sign.Var ( atomically, modifyTVar' )
import Vulk.VulkGLFW ( makeFullscreen, makeWindowed )

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
  -- this will exit the game
  (EventSys SysExit) → do
    st ← get
    case stWindow st of
      Just win → liftIO $ GLFW.setWindowShouldClose win True
      Nothing  → liftIO exitSuccess
  -- recreation of the swapchain, reloading all textures
  (EventSys SysRecreate) → do
    modify $ \s → s { stReload = RSRecreate }
  -- reloading of dynamic data to apply to trans functions
  (EventSys SysReload) → do
    stRel ← gets stReload
    case stRel of
      RSRecreate → return ()
      _          → modify $ \s → s { stReload = RSReload }
  (EventSys SysFullScreen)         → makeFullscreen
  (EventSys (SysWindowed w h x y)) → makeWindowed w h x y
  (EventSys cmd) → logInfo $ "no known sys command" ⧺ show cmd
  -- processing of input occurs in the input thread
  (EventInput (InputKey         win k _ ks mk))
    → evalKey win k ks mk
  (EventInput (InputMouseButton win mb mbs mk))
    → evalMouse win mb mbs mk
  (EventInput (InputMouseScroll win x y      ))
    → evalScroll win x y
  (EventInputState inputStateChange)
    → updateInputState inputStateChange

-- TODO: move this into its own folder and implement settings
processSettingsChange ∷ SettingsChange → Prog ε σ ()
processSettingsChange change = logInfo $ show change
