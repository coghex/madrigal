{-# LANGUAGE BangPatterns #-}
-- | thread-safe variables, typically used for small simple things
--   that need to be fast and in the main draw thread
module Sign.Var where
-- thread safe memory is defined
import Prelude ()
import UPrelude
import qualified Control.Concurrent.STM as STM
import Data.Function ((&))
-- | type synonyms for ease of use
type MVar = STM.TMVar
type TVar = STM.TVar
-- | function synonyms
newTVar     ∷ α → STM.STM (TVar α)
newTVar     = STM.newTVar
readTVar    ∷ TVar α → STM.STM α
readTVar    = STM.readTVar
readTVarIO  ∷ TVar α → IO α
readTVarIO  = STM.readTVarIO
writeTVar   ∷ TVar α → α → STM.STM ()
writeTVar   = STM.writeTVar
-- | taken from a random hackage
--   module i saw. strict,
--   requires bangpatterns
modifyTVar  ∷ TVar α → (α → (α, β)) → STM.STM β
modifyTVar ref f = STM.readTVar ref ⌦ \a
  → f a & \(!a', !b) → STM.writeTVar ref a' ⚞ b
-- | use this one though, the other one is
--   really only if you need read and write
modifyTVar' ∷ TVar α → (α → α) → STM.STM ()
modifyTVar' = STM.modifyTVar'
atomically  ∷ STM.STM α → IO α
atomically  = STM.atomically
