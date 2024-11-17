-- | queues are generic for type but are typically
--   used here in conjunction with ADTs to allow
--   for a messaging system between threads
module Sign.Queue where
-- an event queue is defined
import qualified Control.Concurrent.STM as STM

-- | type synonym for ease of use
type Queue = STM.TQueue
type TChan = STM.TChan

-- | these functions act as synonyms
--   to the STM library
newQueue     ∷ IO (Queue α)
newQueue     = STM.newTQueueIO
writeQueue   ∷ Queue α → α → STM.STM ()
writeQueue   = STM.writeTQueue
tryReadQueue ∷ STM.TQueue α → STM.STM (Maybe α)
tryReadQueue = STM.tryReadTQueue
newTChan     ∷ IO (TChan a)
newTChan     = STM.atomically STM.newTChan
readChan     ∷ STM.TChan a → STM.STM a
readChan     = STM.readTChan
tryReadChan  ∷ STM.TChan a → STM.STM (Maybe a)
tryReadChan  = STM.tryReadTChan
writeChan    ∷ STM.TChan a → a → STM.STM ()
writeChan    = STM.writeTChan
