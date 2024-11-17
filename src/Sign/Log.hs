{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, Strict, GeneralizedNewtypeDeriving, ConstraintKinds, FlexibleContexts, MonoLocalBinds, CPP, ImplicitParams #-}
-- | logging functions for threads
module Sign.Log where
-- we defines functions to let us pass callstacks to logger
-- it has been hacked to provide an interface to the main thread

import Prelude()
import UPrelude
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.Reader
import System.Log.FastLogger
import Elem.Data ( InputAct(..) )
import Prog.Data ( Env(..) )
import Sign.Data ( LogLevel(..), Event(..), TState(..), SysAction(..), LoadData(..), SettingsChange(..) )
import Sign.Var ( atomically, readTVar )
import Sign.Queue ( writeQueue, readChan, tryReadChan, tryReadQueue )
import Load.Data (LoadCmd(..), GameCmd(..))
import Vulk.Font (TTFData(..))
import qualified Vulk.GLFW as GLFW

-- | monadic boilerplate logger from simple-log package on hackage
data Log = Log { logLevel  ∷ LogLevel
               , env       ∷ Env
               , formatter ∷ LogLevel → String → LogStr
               , logFunc   ∷ LogStr → IO ()
               , cleanUp   ∷ IO () } | LogIO

-- | exposes MonadLog class
class (MonadIO μ) ⇒ MonadLog μ where
  askLog   ∷ μ Log
  localLog ∷ (Log → Log) → μ α → μ α

instance {-# OVERLAPPABLE #-} (MonadLog μ, MonadTrans τ, MonadLog IO, MonadFail μ
                              , MFunctor τ, MonadIO (τ μ))
                                  ⇒ MonadLog (τ μ) where
  askLog      = lift askLog
  localLog fn = hoist $ localLog fn

-- | a logger with ReaderT state to remember the event queue
newtype LogT μ α = LogT { runLogT ∷ ReaderT Log μ α }
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadReader Log)

instance MonadTrans LogT where
  lift = LogT ∘ lift

instance (MonadIO μ) ⇒ MonadLog (LogT μ) where
  askLog      = LogT ask
  localLog fn = LogT ∘ local fn ∘ runLogT
instance (MonadLog IO) where
  askLog      = return LogIO
  localLog fn = localLog fn

makeLogger ∷ (MonadIO μ) ⇒ Env → (LogLevel → String → LogStr) → LogType → LogLevel → μ Log
makeLogger env fmt typ lvl = liftIO $ do
  (fl,cl) ← newFastLogger typ
  return $ Log lvl env fmt fl cl

makeDefaultLogger ∷ (MonadIO μ) ⇒ Env → LogType → LogLevel → μ Log
makeDefaultLogger env = makeLogger env defaultFormatter

defaultFormatter ∷ LogLevel → String → LogStr
defaultFormatter _ = toLogStr

-- | runs the reader monad
runLog ∷ (MonadIO μ) ⇒ Log → LogT μ α → μ α
runLog l m = runReaderT (runLogT m) l

-- *** utility functions

-- | raw log, meant for use with the logging functions
log' ∷ (MonadLog μ, MonadFail μ) ⇒ LogLevel → String → μ ()
log' lvl msg = do
  (Log fil env _   _   _) ← askLog
  when (lvlbelow fil lvl) $ liftIO $ do
    atomically $ writeQueue (envEventQ env) $ EventLog lvl msg
    -- this line would use the fast logger, this may be better to use
--    ( fun ∘ toLogStr ) (fmt lvl msg)
-- | checks that the message level is below verbosity
lvlbelow ∷ LogLevel → LogLevel → Bool
lvlbelow (LogDebug n) (LogDebug m) = n ≥ m -- debug level
lvlbelow (LogDebug _) _            = True
lvlbelow _            (LogDebug _) = False
lvlbelow LogInfo      LogInfo      = True
lvlbelow _            LogInfo      = False
lvlbelow _            _            = True
-- | hangs execution until it reads something
readTimerBlocked ∷ (MonadLog μ, MonadFail μ) ⇒ μ TState
readTimerBlocked = do
  (Log _   env _   _   _) ← askLog
  liftIO $ atomically $ readChan (envLoadCh env)
-- | hangs execution until it reads something
readGameTimerBlocked ∷ (MonadLog μ, MonadFail μ) ⇒ μ TState
readGameTimerBlocked = do
  (Log _   env _   _   _) ← askLog
  liftIO $ atomically $ readChan (envGameCh env)
-- | returns nothing if load channel is empty
readTimer ∷ (MonadLog μ, MonadFail μ) ⇒ μ (Maybe TState)
readTimer = do
  (Log _   env _   _   _) ← askLog
  liftIO $ atomically $ tryReadChan (envLoadCh env)
-- | returns nothing if game channel is empty
readGameTimer ∷ (MonadLog μ, MonadFail μ) ⇒ μ (Maybe TState)
readGameTimer = do
  (Log _   env _   _   _) ← askLog
  liftIO $ atomically $ tryReadChan (envGameCh env)
-- | returns nothing if load queue is empty
readCommand ∷ (MonadLog μ, MonadFail μ) ⇒ μ (Maybe LoadCmd)
readCommand = do
  (Log _   env _   _   _) ← askLog
  liftIO $ atomically $ tryReadQueue (envLoadQ env)
-- | returns nothing if game queue is empty
readGameCommand ∷ (MonadLog μ, MonadFail μ) ⇒ μ (Maybe GameCmd)
readGameCommand = do
  (Log _   env _   _   _) ← askLog
  liftIO $ atomically $ tryReadQueue (envGameQ env)
-- | sends a syscommand over the event queue
sendSys ∷ (MonadLog μ, MonadFail μ) ⇒ SysAction → μ ()
sendSys sa = do
  (Log _   env _   _   _) ← askLog
  liftIO $ atomically $ writeQueue (envEventQ env) $ EventSys sa
-- | sends a new settings change to the event queue
sendSettings ∷ (MonadLog μ, MonadFail μ) ⇒ SettingsChange → μ ()
sendSettings sa = do
  (Log _   env _   _   _) ← askLog
  liftIO $ atomically $ writeQueue (envEventQ env) $ EventSettings sa
-- | sends a load command over the load queue
sendLoadCmd ∷ (MonadLog μ, MonadFail μ) ⇒ LoadCmd → μ ()
sendLoadCmd lc = do
  (Log _   env _   _   _) ← askLog
  liftIO $ atomically $ writeQueue (envLoadQ env) lc
-- | sends a game command over the load queue
sendGameCmd ∷ (MonadLog μ, MonadFail μ) ⇒ GameCmd → μ ()
sendGameCmd gc = do
  (Log _   env _   _   _) ← askLog
  liftIO $ atomically $ writeQueue (envGameQ env) gc
-- | reads the current key layout
readFontMapM ∷ (MonadLog μ, MonadFail μ) ⇒ LogT μ (Maybe [TTFData])
readFontMapM = do
  (Log _   env _   _   _) ← askLog
  liftIO $ atomically $ readTVar (envFontM env)
-- | sends a load event over the event queue
sendLoad ∷ (MonadLog μ, MonadFail μ) ⇒ LoadData → LogT μ ()
sendLoad cmd = do
  (Log _   env _   _   _) ← askLog
  liftIO $ atomically $ writeQueue (envEventQ env) $ EventLoad cmd
-- | sends a input action over the input queue
sendInpAct ∷ (MonadLog μ, MonadFail μ) ⇒ InputAct → LogT μ ()
sendInpAct cmd = do
  (Log _   env _   _   _) ← askLog
  liftIO $ atomically $ writeQueue (envInpQ env) cmd
-- | preforms necessary work to toggle fullscreen
toggleFullScreen ∷ (MonadLog μ, MonadFail μ) ⇒ Maybe (Int,Int) → LogT μ (Maybe (Int,Int))
toggleFullScreen oldSize = do
  (Log _   env _   _   _) ← askLog
  m' ← liftIO GLFW.getPrimaryMonitor
  w' ← liftIO $ atomically $ readTVar (envWindow env)
  case w' of
    Nothing → log' LogError "no glfw window present" ≫ return Nothing
    Just w0 → do
      case m' of
        Nothing → log' LogError "no primary monitor" ≫ return Nothing
        Just m0 → do
          vm' ← liftIO $ GLFW.getVideoMode m0
          case vm' of
            Nothing → log' LogError "no primary video mode" ≫ return Nothing
            Just _  → case oldSize of
              Nothing → do
                (ow,oh) ← liftIO $ GLFW.getWindowSize w0
                liftIO $ atomically $ writeQueue (envEventQ env) $ EventSys SysFullScreen
                return $ Just (ow,oh)
              Just (w,h) → do
                liftIO $ atomically $ writeQueue (envEventQ env) $ EventSys $ SysWindowed w h 10 10
                return Nothing
