-- | various ADTs for handling events, input and load data
module Sign.Data where
-- data for the main event queue is defined
import Prelude()
import UPrelude
import Data ( PrintArg(..), KeyMap(..), KeyFunc(..) )

-- | timer state is used for all the timers
data TState = TStart | TStop | TPause | TNULL deriving (Show, Eq)

-- | events processed by the main thread
data Event = EventSys !SysAction

-- | log levels are for monadic logger, but stdio
data LogLevel = LogDebug Int
              | LogInfo
              | LogWarn
              | LogPrint PrintArg
              | LogError
              | LogNULL deriving (Show, Eq)

-- | commands for functionality at the lowest level
data SysAction = SysRecreate | SysReload
               | SysFullScreen
               | SysWindowed Int Int Int Int
               | SysExit | SysNULL deriving (Show, Eq)
