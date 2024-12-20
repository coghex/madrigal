{-# LANGUAGE OverloadedLists #-}
module Vulk.Loop where
import Prelude()
import UPrelude
import Data.Maybe ( fromMaybe )
import Data.Semigroup ( Arg(..), ArgMin, Min(Min, getMin))
import qualified Data.Vector as V
import Data.Word ( Word32(..), Word64(..) )
import Prog ( Prog(..) )
import Prog.Util ( allocResource, logExcept )
import Sign.Except ( ExType(..) )
import Vulk.Data ( DevQueues(..), SwapchainInfo(..)
                 , SwapchainSupportDetails(..), VulkResult(..) )
import Vulk.Device ( querySwapchainSupport )
import Vulkan.CStruct.Extends
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_swapchain as Swap
import Vulkan.Extensions.VK_KHR_surface as Surf
import Vulkan.Zero

-- | reacreates the swapchain if needed
createVulkanSwapchain ∷ PhysicalDevice → Device → DevQueues → SurfaceKHR
  → Prog ε σ SwapchainInfo
createVulkanSwapchain pdev dev queues surface = do
  SwapchainSupportDetails{..} ← querySwapchainSupport pdev surface
  let ssd = SwapchainSupportDetails{..}
      SurfaceFormatKHR{format=form,colorSpace=cs} = chooseSwapSurfaceFormat ssd
  let imageCount = min (Surf.maxImageCount capabilities)
                       (Surf.minImageCount capabilities)
      spMode     = chooseSwapPresentMode ssd
      sExtent    = chooseSwapExtent      ssd
      (sharing, qfi) = if (graphicsQueue queues
                           ≠ presentQueue queues)
                        then (SHARING_MODE_CONCURRENT
                             , V.fromList [ graphicsFamIdx queues
                                          , presentFamIdx queues])
                        else (SHARING_MODE_EXCLUSIVE, [])
      swCreateInfo = zero
        { surface            = surface
        , minImageCount      = imageCount
        , imageFormat        = form
        , imageColorSpace    = cs
        , imageExtent        = sExtent
        , imageArrayLayers   = 1
        , imageUsage         = IMAGE_USAGE_COLOR_ATTACHMENT_BIT
        , imageSharingMode   = sharing
        , queueFamilyIndices = qfi
        , preTransform       = currentTransform capabilities
        , compositeAlpha     = COMPOSITE_ALPHA_OPAQUE_BIT_KHR
        , presentMode        = spMode
        , clipped            = True
        , oldSwapchain       = zero
        }
  swapchain ← allocResource (\s0 → destroySwapchainKHR dev s0 Nothing)
    $ (createSwapchainKHR dev swCreateInfo Nothing)
  (_,swapImgs) ← getSwapchainImagesKHR dev swapchain
  return $ SwapchainInfo { swapchain      = swapchain
                         , swapImgs       = swapImgs
                         , swapImgFormat  = form
                         , swapExtent     = sExtent }

-- | use the best swap surface format
chooseSwapSurfaceFormat ∷ SwapchainSupportDetails → SurfaceFormatKHR
chooseSwapSurfaceFormat (SwapchainSupportDetails _ formats _) = best
  where best = if preferred `elem` formats then preferred else V.head formats
        preferred = zero { format = FORMAT_B8G8R8A8_UNORM
                         , colorSpace = COLOR_SPACE_SRGB_NONLINEAR_KHR }
  

-- | use the best swap present mode
chooseSwapPresentMode ∷ SwapchainSupportDetails → PresentModeKHR
chooseSwapPresentMode (SwapchainSupportDetails _ _ presModes) = best
  where best = if   PRESENT_MODE_IMMEDIATE_KHR `elem` presModes
               then PRESENT_MODE_IMMEDIATE_KHR
               else if PRESENT_MODE_MAILBOX_KHR `elem` presModes
               then PRESENT_MODE_MAILBOX_KHR
               else if PRESENT_MODE_FIFO_KHR `elem` presModes
               then PRESENT_MODE_FIFO_KHR
               else V.head presModes

-- | set the width and height of the swapchain
chooseSwapExtent ∷ SwapchainSupportDetails → Extent2D
chooseSwapExtent SwapchainSupportDetails{..} = zero
  { width  = ( max (minw) $ min (maxw) (curw) )
  , height = ( max (minh) $ min (maxh) (curh) ) }
  where Extent2D{width=minw,height=minh} = minImageExtent capabilities
        Extent2D{width=maxw,height=maxh} = maxImageExtent capabilities
        Extent2D{width=curw,height=curh} = currentExtent  capabilities

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
