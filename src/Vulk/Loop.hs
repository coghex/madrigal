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
  return $ SwapchainInfo { siSwapchain      = swapchain
                         , siSwapImgs       = swapImgs
                         , siSwapImgFormat  = form
                         , siSwapExtent     = sExtent }

-- | creates image views for swapchain images
createSwapchainImageViews ∷ Device → SwapchainInfo → Prog ε σ (V.Vector ImageView)
createSwapchainImageViews dev SwapchainInfo{..} = do
  V.mapM createImageViewf siSwapImgs
  where
    createImageViewf image = 
      allocResource (\iv → destroyImageView dev iv Nothing) $
        createImageView dev zero
          { image = image
          , viewType = IMAGE_VIEW_TYPE_2D
          , format = siSwapImgFormat
          , components = zero
              { r = COMPONENT_SWIZZLE_IDENTITY
              , g = COMPONENT_SWIZZLE_IDENTITY
              , b = COMPONENT_SWIZZLE_IDENTITY
              , a = COMPONENT_SWIZZLE_IDENTITY }
          , subresourceRange = zero
              { aspectMask = IMAGE_ASPECT_COLOR_BIT
              , baseMipLevel = 0
              , levelCount = 1
              , baseArrayLayer = 0
              , layerCount = 1 }
          } Nothing

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

createDescriptorSets ∷ Device → DescriptorPool → DescriptorSetLayout
  → ImageView → Sampler → Prog ε σ DescriptorSet
createDescriptorSets dev descPool descSetLayout imageView sampler = do
  let allocInfo = zero { descriptorPool = descPool
                       , setLayouts = [descSetLayout] }
  descSets ← allocateDescriptorSets dev allocInfo
  let descSet = V.head descSets
  let imageInfo = zero { imageView = imageView
                       , imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                       , sampler = sampler }
      writeDescSet = zero { dstSet = descSet
                          , dstBinding = 0
                          , dstArrayElement = 0
                          , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                          , descriptorCount = 1
                          , imageInfo = [imageInfo] }
  updateDescriptorSets dev [SomeStruct writeDescSet] []
  return descSet

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
