-- | various ADTs for handling events, input and load data
module Sign.Data where
-- data for the main event queue is defined
import Prelude()
import UPrelude
import Data ( PrintArg(..), KeyMap(..), KeyFunc(..) )
import qualified Vulk.GLFW as GLFW

-- | timer state is used for all the timers
data TState = TStart | TStop | TPause | TNULL deriving (Show, Eq)

-- | events processed by the main thread
data Event = EventError !GLFW.Error !String -- GLFW specific
           -- | logs into the monadic logger, also allows stdout
           | EventLog !LogLevel !String
           -- | changes to the state of the input
           | EventInputState !InputStateChange
           -- | key/mouse input
           | EventInput !InputEvent
           -- | lowest level actions go here
           | EventSys !SysAction

-- | log levels are for monadic logger, but stdio
data LogLevel = LogDebug Int
              | LogInfo
              | LogWarn
              | LogPrint PrintArg
              | LogError
              | LogNULL deriving (Show, Eq)

-- | input state can be changed by sending event
data InputStateChange = ISCKeyPress !KeyFunc
                      | ISCKeyRelease !KeyFunc
                      | ISCAccelerate !(Double,Double)
                      | ISCResetCam
                      | ISCNULL deriving (Show, Eq)

-- | commands for functionality at the lowest level
data SysAction = SysRecreate | SysReload
               | SysFullScreen
               | SysWindowed Int Int Int Int
               | SysTest TestParam
               | SysExit | SysNULL deriving (Show, Eq)

-- | system testing parameters
data TestParam
  = TPWindow | TPNetwork
  | TPNULL deriving (Show, Eq)

-- | input sources enumerated
data InputEvent
  = InputKey !GLFW.Window !GLFW.Key !Int !GLFW.KeyState
      !GLFW.ModifierKeys
  | InputMouseButton !GLFW.Window !GLFW.MouseButton
      !GLFW.MouseButtonState !GLFW.ModifierKeys
  | InputMouseScroll !GLFW.Window !Double !Double

