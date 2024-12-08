{-# LANGUAGE OverloadedLists #-}
module Vulk.Loop where
import Prelude()
import UPrelude
import qualified Data.Vector as V
import Data.Word ( Word32(..), Word64(..) )
import Prog ( Prog(..) )
import Vulkan.CStruct.Extends
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Zero

acquireVulkanImage ∷ Device → SwapchainKHR → Word64 → Semaphore
  → Prog ε σ Word32
acquireVulkanImage dev swapchain maxBound imageAvailableSemaphore = do
  res ← acquireNextImageKHR dev swapchain maxBound imageAvailableSemaphore zero
  return $ snd res

submitQueue ∷ SwapchainKHR → Semaphore → Semaphore → Word32
  → Queue → Queue → V.Vector CommandBuffer → Prog ε σ ()
submitQueue swapchain imageAvailableSemaphore renderFinishedSemaphore
            imageIndex graphicsQueue presentQueue commandBuffers = do
  let submitInfo = zero
        { waitSemaphores = [imageAvailableSemaphore]
        , waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
        , commandBuffers   = [ commandBufferHandle
                               $   commandBuffers
                               V.! fromIntegral imageIndex ]
        , signalSemaphores = [renderFinishedSemaphore]
        }
      presentInfo = zero { waitSemaphores = [renderFinishedSemaphore]
                         , swapchains     = [swapchain]
                         , imageIndices   = [imageIndex]
                         }
  queueSubmit graphicsQueue [SomeStruct submitInfo] zero
  _ ← queuePresentKHR presentQueue presentInfo
  pure ()

waitIdle ∷ Device → Queue → Prog ε σ ()
waitIdle dev gq = do
  queueWaitIdle  gq
  deviceWaitIdle dev
