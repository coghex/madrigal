{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
-- | low level memory functions for vulkan data
module Vulk.Foreign where
-- vulkan specific pointer functions
import Prelude()
import UPrelude
import Control.Monad (when)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Word ( Word32 )
import Foreign.Ptr
import qualified Foreign.Marshal.Array as Foreign
import Foreign.Storable (Storable)
import qualified Foreign.Storable as Storable
import GHC.Stack ( HasCallStack, prettyCallStack, callStack )
import Sign.Except ( ProgExcept(ProgExcept), ExType(ExVulk) )
import Prog
    ( MonadIO(liftIO),
      MonadError(throwError),
      MonadState(state),
      Prog(..),
      Prog' )
import Prog.Data ( State(stStatus) )
import Prog.Foreign ( alloca, allocaArray, liftIOWith, touch )
import Vulkan.Core10

instance Enum Result where
  fromEnum SUCCESS = 0
  fromEnum _          = 1
  toEnum   0          = SUCCESS
  toEnum   _          = ERROR_UNKNOWN


-- | runs io vulkan command,
--   throwing unique exception
runVk ∷ HasCallStack ⇒ IO Result → Prog ε σ ()
runVk action = do
  r ← liftIO action
  let ret = ProgExcept (Just r) ExVulk
              $ "vulkan command returned error: "
              ⧺ show r ⧺ "\n\n" ⧺ prettyCallStack callStack
  state $ \s → ((), s { stStatus = ret })
  when (r < SUCCESS) $ throwError ret
{-# INLINE runVk #-}
