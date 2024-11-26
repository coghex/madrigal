{-# LANGUAGE StrictData #-}
-- | initialization of many things, including state,
--   env, drawsstate, inputsttate, settings, and
--   the keymapping, among others...
module Prog.Init where
import Prelude()
import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import Data.Time.Clock.System ( getSystemTime )
import qualified Data.Map as Map
import GHC.Stack ( HasCallStack )
import Data ( FPS(..), Key(..), KeyMap(..), KeyFunc(..) )
import Prog ( Prog(unProg) )
import Prog.Data ( Env(..), State(..)
                 , ISKeys(..), InputState(..)
                 , ProgResult(..), ISStatus(..) )
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
  eventQ  ← newQueue
  inpQ    ← newQueue
  inputCh ← newTChan
  netQ    ← newQueue
  netCh   ← newTChan
  win     ← atomically $ newTVar Nothing
  let env = Env { envEventQ   = eventQ
                , envInpQ     = inpQ
                , envInpCh    = inputCh
                , envNetQ     = netQ
                , envNetCh    = netCh
                , envWindow   = win }
  envChan ← atomically $ newTVar env
  return (envChan, env)

-- | state is best for things that change often
initState ∷ Env → IO (TVar State)
initState _   = do
  -- the status handles errors
  let ref = ProgExcept (Just ProgSuccess) ExProg ""
  -- initial input state is empty
      is  = initInpState
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  st ← getSystemTime
  atomically $ newTVar State { stStatus     = ref
                             , stLogFunc    = lf
                             , stWindow     = Nothing
                             , stInput      = is
                             , stStartT     = st
                             , stFPS        = FPS 60.0 60
                             , stTick       = Nothing
                             , stInstance   = Nothing
                             , stDebugMsg   = Nothing
                             , stSurface    = Nothing
                             , stDevice     = Nothing
                             , stSwapchain  = Nothing
                             , stImgViews   = Nothing
                             , stRenderPass = Nothing
                             }

-- | initial empty input state
initInpState ∷ InputState
initInpState = InputState { inpStatus = ISSNULL
                          , mouse1    = Nothing
                          , mouse2    = Nothing
                          , mouse3    = Nothing
                          , mousePos  = (0,0)
                          , keySt     = initKS }
  where initKS = ISKeys { keyUp    = False
                        , keyLeft  = False
                        , keyDown  = False
                        , keyRight = False
                        , keyAccel = (0,0) }

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
