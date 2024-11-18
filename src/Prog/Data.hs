-- | data for the continuation monad
{-# LANGUAGE RankNTypes #-}
module Prog.Data where
-- the state is
import Prelude()
import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import Data.Time.Clock.System ( SystemTime )
import Data ( FPS(..) )
import Sign.Data ( Event, TState, InputAct )
import Sign.Except ( ProgExcept )
import Sign.Queue ( Queue, TChan )
import Sign.Var ( TVar )
import qualified Vulk.GLFW as GLFW

-- | specific utility actions
data ProgResult = ProgSuccess | ProgError deriving (Show, Eq)
-- | glfw loop status
data LoopControl = ContinueLoop | AbortLoop deriving Eq

-- | env holds only pointers
data Env = Env { envEventQ ∷ Queue Event
               , envInpQ   ∷ Queue InputAct
               , envInpCh  ∷ TChan TState
               -- only use this one for reads i think
               , envWindow ∷ TVar (Maybe GLFW.Window) }

data State = State { stStatus   ∷ ProgExcept
                   , stLogFunc  ∷ Logger.Loc → Logger.LogSource
                                    → Logger.LogLevel → Logger.LogStr
                                    → IO ()
                   , stWindow   ∷ !(Maybe GLFW.Window)
                   , stInput    ∷ !InputState
                   , stStartT   ∷ !SystemTime
                   , stFPS      ∷ !FPS
                   , stTick     ∷ !(Maybe Double) }

-- | input state for the main thread only
data InputState = InputState { inpStatus ∷ ISStatus
                             , mouse1    ∷ Maybe (Double,Double)
                             , mouse2    ∷ Maybe (Double,Double)
                             , mouse3    ∷ Maybe (Double,Double)
                             , mousePos  ∷ (Double,Double)
                             , keySt     ∷ ISKeys
                             } deriving (Show, Eq)

-- | return status for the input thread
data ISStatus = ISSLogDebug String
              | ISSNULL deriving (Show, Eq)

-- | certain keys state
data ISKeys = ISKeys { keyUp    ∷ Bool
                     , keyLeft  ∷ Bool
                     , keyDown  ∷ Bool
                     , keyRight ∷ Bool
                     , keyAccel ∷ (Double,Double)
                     } deriving (Show, Eq)
