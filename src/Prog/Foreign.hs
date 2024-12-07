{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedTuples #-}
-- | here foreign just means stuff that deals with memory
--   the vulkan data is all stored as dataframes. some
--   of these functions may not be exactly portable
module Prog.Foreign where
-- interface to some of the lower level c-like functions
import Prelude()
import UPrelude
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception ( bracket )
import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Array as Foreign
import Foreign.Ptr ( Ptr, plusPtr )
import Foreign.Storable (Storable)
import qualified Foreign.Storable as Storable
import qualified GHC.Base as GHC
import Prog ( MonadIO(liftIO), Prog(..), Prog' )
import Sign.Except ( ProgExcept )
-- | read value from location in memory
peek ∷ Storable α ⇒ Ptr α → Prog ε σ α
peek = liftIO ∘ Storable.peek
-- | write the given value to the location in memory
poke ∷ Storable α ⇒ Ptr α → α → Prog ε σ ()
poke p v = liftIO $ Storable.poke p v
-- | executes a function in monadic context with an allocated
--   block of memory for values of type α
alloca ∷ Storable α ⇒ (Ptr α → Prog' ε β) → Prog ε σ β
alloca = liftIOWith Foreign.alloca
-- | similar to above but returs the original input value type
allocaPeek ∷ Storable α
  ⇒ (Ptr α → Prog ε (Either ProgExcept α) ()) → Prog ε σ α
allocaPeek f = alloca $ \ptr → f ptr ≫ liftIO (Storable.peek ptr)
-- | like alloca but for multiple elements
allocaArray ∷ Storable α ⇒ Int → (Ptr α → Prog' ε β) → Prog ε σ β
allocaArray = liftIOWith ∘ Foreign.allocaArray
-- | convert a c array in memory to a haskell list
peekArray ∷ Storable α ⇒ Int → Ptr α → Prog ε σ [α]
peekArray n = liftIO ∘ Foreign.peekArray n
-- | unwraps an IO operation and lifts the given function
liftIOWith ∷ ((α → IO (Either ProgExcept β))
  → IO (Either ProgExcept β)) → (α → Prog' ε β) → Prog ε σ β
liftIOWith iof pf
  = Prog $ \e s c → iof (\a → unProg (pf a) e s pure) ⌦ c
{-# INLINE liftIOWith #-}
-- | Res functions release memory after use
mallocArrayRes ∷ Storable α ⇒ Int → Prog ε σ (Ptr α)
mallocArrayRes n = Prog $ \_ _ c → Foreign.allocaArray n (c ∘ Right)
-- | malloc function behave mostly as they do
--   in c, but wrapped up in monad
mallocRes ∷ Storable α ⇒ Prog ε σ (Ptr α)
mallocRes = Prog $ \_ _ c → Foreign.alloca (c ∘ Right)
-- | temporarily stores an array, then returns it
newArrayRes ∷ Storable α ⇒ [α] → Prog ε σ (Ptr α)
newArrayRes xs = Prog $ \_ _ c → Foreign.withArray xs (c ∘ Right)
-- | return pointer from dataframes
ptrAtIndex ∷ ∀ α. Storable α ⇒ Ptr α → Int → Ptr α
ptrAtIndex ptr i = ptr `plusPtr` (i * Storable.sizeOf @α undefined)
-- | prevents garbage collection
touch ∷ α → IO ()
touch x = GHC.IO $ \s → case GHC.touch# x s of s' → (# s', () #)
{-# INLINE touch #-}
