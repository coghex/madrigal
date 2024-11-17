{-# LANGUAGE StrictData #-}
-- | initialization of many things, including state,
--   env, drawsstate, inputsttate, settings, and
--   the keymapping, among others...
module Prog.Init
  ( runProg ) where
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
    ( Env(..), State(..), ProgResult(..) )
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
  let env = Env { envEventQ = eventQ }

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
  -- the logger provides multiple levels of warnings/info
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  -- the system time marks the start of execution
  st ← getSystemTime
  -- TODO: load settings from file
  -- state is accessed transactionally
  atomically $ newTVar State { stStatus    = ref
                             , stLogFunc   = lf
                             , stStartT    = st }
