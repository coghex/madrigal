-- | data for the continuation monad
{-# LANGUAGE RankNTypes #-}
module Prog.Data where
-- the state is
import Prelude()
import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import Data.Time.Clock.System ( SystemTime )
import Data ( FPS(..), KeyFunc(..), Key(..) )
import Net.Data ( NetAction(..) )
import Sign.Data ( Event, TState )
import Sign.Except ( ProgExcept )
import Sign.Queue ( Queue, TChan )
import Sign.Var ( TVar )
import qualified Vulk.GLFW as GLFW
import qualified Vulkan.Core10 as VK
import qualified Vulkan.Extensions.Handles as VK

-- | specific utility actions
data ProgResult = ProgSuccess | ProgError deriving (Show, Eq)
-- | glfw loop status
data LoopControl = ContinueLoop | AbortLoop deriving (Show, Eq)

-- | env holds only pointers
data Env = Env { envEventQ   ∷ Queue Event
               , envInpQ     ∷ Queue InputAct
               , envInpCh    ∷ TChan TState
               , envNetQ     ∷ Queue NetAction
               , envNetCh    ∷ TChan TState
               -- only use these ones for reads i think
               , envWindow   ∷ TVar (Maybe GLFW.Window) }

data State = State { stStatus    ∷ ProgExcept
                   , stLogFunc   ∷ Logger.Loc → Logger.LogSource
                                     → Logger.LogLevel → Logger.LogStr
                                     → IO ()
                   , stWindow    ∷ !(Maybe GLFW.Window)
                   , stInput     ∷ !InputState
                   , stStartT    ∷ !SystemTime
                   , stFPS       ∷ !FPS
                   , stTick      ∷ !(Maybe Double)
                   , stInstance  ∷ Maybe VK.Instance
                   , stDebugMsg  ∷ Maybe VK.DebugUtilsMessengerEXT
                   , stSurface   ∷ Maybe VK.SurfaceKHR
                   , stDevice    ∷ Maybe VK.Device
                   , stSwapchain ∷ Maybe VK.SwapchainKHR }

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

-- | possible return types
data InpResult = ResInpSuccess | ResInpError String
               | ResInpState InputState
               | ResInpChangeKey KeyFunc Key Int | ResInpNULL

-- | certain keys state
data ISKeys = ISKeys { keyUp    ∷ Bool
                     , keyLeft  ∷ Bool
                     , keyDown  ∷ Bool
                     , keyRight ∷ Bool
                     , keyAccel ∷ (Double,Double)
                     } deriving (Show, Eq)

-- | possible input actions
data InputAct  = InpActKey GLFW.Key GLFW.KeyState GLFW.ModifierKeys 
               | InpActTest
               | InpActNULL deriving (Show, Eq)

