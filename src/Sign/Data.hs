-- | various ADTs for handling events, input and load data
module Sign.Data where
-- data for the main event queue is defined
import Prelude()
import UPrelude
import Data ( PrintArg(..), KeyMap(..), KeyFunc(..) )

-- | timer state is used for all the timers
data TState = TStart | TStop | TPause | TNULL deriving (Show, Eq)

-- | events processed by the main thread
data Event = EventLog !LogLevel !String
           -- | changes to the state of the input
           | EventInputState !InputStateChange
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
               | SysExit | SysNULL deriving (Show, Eq)

-- | possible input actions
data InputAct = InpActNULL deriving (Show, Eq)
