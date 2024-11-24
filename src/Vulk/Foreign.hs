{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
module Vulk.Foreign where
import Prelude()
import UPrelude
import Control.Monad ( when )
import Control.Concurrent.MVar ( newEmptyMVar, putMVar, takeMVar )
import Data.Word ( Word32 )
import qualified Foreign.Marshal.Array as Foreign
import Foreign.Storable ( Storable )
import qualified Foreign.Storable as Storable
import Foreign.Ptr ( Ptr )
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
import qualified Vulkan.Core10 as VK

instance Enum VK.Result where
  fromEnum VK.SUCCESS = 0
  fromEnum _          = 1
  toEnum   0          = VK.SUCCESS
  toEnum   _          = VK.ERROR_UNKNOWN

-- | runs io vulkan command,
--   throwing unique exception
runVk ∷ HasCallStack ⇒ IO VK.Result → Prog ε σ ()
runVk action = do
  r ← liftIO action
  let ret = ProgExcept (Just r) ExVulk
              $ "vulkan command returned error: "
              ⧺ show r ⧺ "\n\n" ⧺ prettyCallStack callStack
  state $ \s → ((), s { stStatus = ret })
  when (r < VK.SUCCESS) $ throwError ret
{-# INLINE runVk #-}
