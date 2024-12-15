{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Vulk.Command where
import Prelude()
import UPrelude
import Data.Word ( Word64(..) )
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Prog ( Prog(..) )
import Prog.Util ( allocResource, locally )
import Prog.Foreign ( newArrayRes )
import Vulk.Foreign ( runVk )
import Vulkan.CStruct.Extends
import Vulkan.Core10
import Vulkan.Zero

-- | runs commands locally in a command buffer then returns the result
runCommandsOnce ∷ Device → CommandPool → Queue
  → (CommandBuffer → Prog ε σ α) → Prog ε σ α
runCommandsOnce dev commandPool cmdQueue action = do
  let allocInfo = zero { level              = COMMAND_BUFFER_LEVEL_PRIMARY
                       , commandPool        = commandPool
                       , commandBufferCount = 1 }
  buffer' ← allocResource (freeCommandBuffers dev commandPool)
    $ allocateCommandBuffers dev allocInfo
  let buffer = V.head buffer'
      buffer'' = commandBufferHandle buffer
      cmdbBI = zero { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
                    , inheritanceInfo = Nothing }
  beginCommandBuffer buffer cmdbBI
  result ← action buffer
  endCommandBuffer buffer
  let submitInfo = zero
                          { waitSemaphores   = []
                          , waitDstStageMask = []
                          , commandBuffers   = V.singleton buffer''
                          , signalSemaphores = [] }
  locally $ do
    fence ← createFence dev zero Nothing
    queueSubmit cmdQueue [SomeStruct submitInfo] fence
    let fencePtr = V.singleton fence
    waitForFences dev fencePtr True (maxBound ∷ Word64)
  return result
