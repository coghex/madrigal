-- | logging functions for threads
module Sign.Util where
-- we defines functions to let us pass callstacks to logger
import Prelude()
import UPrelude
import GHC.Stack ( HasCallStack )
import Prog.Data
  ( Env(..) )
import Sign.Data (LogLevel(..), Event (EventLog))
import Sign.Var (atomically)
import Sign.Queue (writeQueue)

-- | when a thread logs something we take the callstack and
--   send it over the event queue
log ∷ HasCallStack ⇒ Env → LogLevel → String → IO ()
log env loglevel str = do
  atomically $ writeQueue (envEventQ env) $ EventLog loglevel str
