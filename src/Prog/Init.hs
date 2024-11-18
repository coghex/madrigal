{-# LANGUAGE StrictData #-}
-- | initialization of many things, including state,
--   env, drawsstate, inputsttate, settings, and
--   the keymapping, among others...
module Prog.Init where
-- initialization of env and state occurs
import Prelude()
import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import Data ( Color(..) )
import qualified Data.Map as Map
import Data.Time.Clock.System ( getSystemTime )
import GHC.Stack ( HasCallStack) -- , prettyCallStack, callStack )
import Data
    ( Key(..), KeyFunc(..), LoadState(..),
      KeyMap(..), Difficulty(..),
      FPS(..), Shell(..), MapType(..), MapTile(..), MapTiles(..) )
import Prog ( Prog(unProg) )
import Prog.Data
import Sign.Except ( ExType(ExProg), ProgExcept(ProgExcept) )
import Sign.Queue ( newQueue, newTChan )
import Sign.Var ( atomically, newTVar, TVar )

-- | the entire monad is unraveled here, after the init functions
runProg ∷ HasCallStack ⇒ (Either ProgExcept α → IO σ)
  → Prog ε σ α → IO σ
runProg c p = do
  (envchan,env) ← initEnv
  st            ← initState env
  unProg p envchan st c

-- | read-only env is best for channels, queues, and other pointers
initEnv ∷ IO (TVar Env, Env)
initEnv = do
  -- event queues handles events from main thread, the
  -- event thread commands are for the main draw thread
  eventQ   ← newQueue
  inpQ     ← newQueue
  inpCh    ← newTChan
  let env = Env { envEventQ = eventQ
                , envInpQ   = inpQ
                , envInpCh  = inpCh }

  -- and env that can be accessed transactionally
  envChan ← atomically $ newTVar env
  -- we return both so that initState doesnt need to load the TVar
  return (envChan, env)

-- | state is bes for things that change often and need to be accessed
--   often verts and dyns, and some other more time critical things are
--   kepts as transactional data referenced in the env.
initState ∷ Env → IO (TVar State)
initState _   = do
  -- the status handles errors
  let ref = ProgExcept (Just ProgSuccess) ExProg ""
  -- initial input state empty
      is  = initInpState
  -- the logger provides multiple levels of warnings/info
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  -- the system time marks the start of execution
  st ← getSystemTime
  -- TODO: load settings from file
  -- state is accessed transactionally
  atomically $ newTVar State { stStatus    = ref
                             , stLogFunc   = lf
                             , stWindow    = Nothing
                             , stFPS       = FPS 60.0 60 True
                             , stInput     = is
                             , stStartT    = st 
                             , stReload    = RSNULL }


-- | creates an empty input state
initInpState ∷ InputState
initInpState = InputState { inpStatus = ISSNULL
                          , mouse1    = Nothing
                          , mouse2    = Nothing
                          , mouse3    = Nothing
                          , mousePos  = (0,0)
                          , isWin     = "win1"
                          , isPage    = "menu1"
                          , isHalt    = HaltNULL
                          , inpCap    = CapNULL
                          , accelCap  = False
                          , keySt     = initKS }
    where initKS = ISKeys { keyUp     = False
                          , keyLeft   = False
                          , keyDown   = False
                          , keyRight  = False
                          , keyAccel  = (0,0) }

-- | creates the base key mapping
-- TODO: load this from a file
initKeyMap ∷ KeyMap
initKeyMap = KeyMap $ Map.fromList
  [(KFEscape,[KeyEscape])
  ,(KFReturn,[KeyReturn])
  ,(KFScrollUp,[KeyW,KeyUp])
  ,(KFScrollDown,[KeyS,KeyDown])
  ,(KFScrollLeft,[KeyA,KeyLeft])
  ,(KFScrollRight,[KeyD,KeyRight])
  ,(KFShell,[KeyTilde])
  ,(KFFullScreen,[KeyF11])
  ,(KFTest,[KeyT]), (KFTest2,[KeyI])]
