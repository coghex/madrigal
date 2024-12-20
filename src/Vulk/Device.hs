{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Vulk.Device where
import Prelude()
import UPrelude
import Control.Monad ( when, guard, (=<<) )
import Control.Monad.Trans.Maybe
import Control.Monad.State.Class ( modify )
import Data.Bits ( Bits(..) )
import qualified Data.ByteString as BS
import Data.List ( nub )
import qualified Data.Map as Map
import Data.Ord ( comparing )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Traversable ( for )
import qualified Data.Vector as V
import Data.Word ( Word32, Word64 )
import System.Exit ( exitFailure )
import Prog ( Prog, MonadIO(liftIO) )
import Prog.Data ( State(..) )
import Prog.Foreign ( newArrayRes )
import Prog.Util ( isDev, logExcept, logInfo, allocResource )
import Vulk.Data ( DevQueues(..), SwapchainSupportDetails(..) )
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_KHR_surface
import qualified Vulkan.Extensions.VK_KHR_surface as SF
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Extensions.VK_KHR_portability_subset
import qualified Vulkan.Extensions.VK_KHR_swapchain as SW
import Vulkan.Core10
import qualified Vulkan.Core10.DeviceInitialization as DI
import Vulkan.Zero


-- | this ends up being where we get the window size from, since the
--   swapchain only supports the max window size
querySwapchainSupport ∷ PhysicalDevice → SurfaceKHR
  → Prog ε σ SwapchainSupportDetails
querySwapchainSupport pdev surf = do
  capabilities     ← getPhysicalDeviceSurfaceCapabilitiesKHR pdev surf
  (_,formats)      ← getPhysicalDeviceSurfaceFormatsKHR      pdev surf
  (_,presentModes) ← getPhysicalDeviceSurfacePresentModesKHR pdev surf
  return SwapchainSupportDetails {..}

-- | creates abstract device from physical one
createGraphicalDevice ∷ Instance → SurfaceKHR
  → Prog ε σ ( PhysicalDevice, Device, DevQueues)
createGraphicalDevice inst surface = do
  let requiredDeviceExtensions = [KHR_SWAPCHAIN_EXTENSION_NAME
                                 ,KHR_PORTABILITY_SUBSET_EXTENSION_NAME] -- macOS
  (pdev, gqfamind, pqfamind, surfaceFormat, presentMode, surfaceCaps)
    ← pickGraphicalPhysicalDevice inst surface requiredDeviceExtensions
                                  (SurfaceFormatKHR FORMAT_B8G8R8_UNORM COLORSPACE_SRGB_NONLINEAR_KHR)
  props ← getPhysicalDeviceProperties pdev
  feats ← getPhysicalDeviceFeatures   pdev
  logInfo $ "using device: " ⧺ (show $ decodeUtf8 $ deviceName props)
  let deviceCreateInfo ∷ DeviceCreateInfo '[]
      deviceCreateInfo = zero
        { queueCreateInfos     = V.fromList
          [ SomeStruct $ zero { queueFamilyIndex = i, queuePriorities = [1] }
          | i ← nub [gqfamind, pqfamind]
          ]
        , enabledExtensionNames = requiredDeviceExtensions
        , enabledFeatures       = Just deviceFeatures
        }
      deviceFeatures = zero { samplerAnisotropy = True }
  dev           ← allocResource destroyVulkanDevice
                    $ createDevice pdev deviceCreateInfo Nothing
  graphicsQueue ← getDeviceQueue dev gqfamind 0
  presentQueue  ← getDeviceQueue dev pqfamind 0
  return (pdev, dev, DevQueues graphicsQueue presentQueue gqfamind pqfamind)
  --let swapchainCreateInfo ∷ SwapchainCreateInfoKHR '[]
  --    swapchainCreateInfo =
  --      let (sharingMode, queueFamilyIndices) = if graphicsQueue ≡ presentQueue
  --            then (SHARING_MODE_EXCLUSIVE, [])
  --            else (SHARING_MODE_CONCURRENT
  --                 , [gqfamind, pqfamind]
  --                 )
  --      in zero { surface            = surface
  --              , minImageCount      = SF.minImageCount surfaceCaps + 1
  --              , imageFormat        = SF.format surfaceFormat
  --              , imageColorSpace    = SF.colorSpace surfaceFormat
  --              , imageExtent        = case currentExtent
  --                                            (surfaceCaps ∷ SurfaceCapabilitiesKHR) of
  --                                              Extent2D w h | w ≡ maxBound, h ≡ maxBound →
  --                                                Extent2D (fromIntegral windowWidth)
  --                                                         (fromIntegral windowHeight)
  --                                              e → e
  --              , imageArrayLayers   = 1
  --              , imageUsage         = IMAGE_USAGE_COLOR_ATTACHMENT_BIT
  --              , imageSharingMode   = sharingMode
  --              , queueFamilyIndices = queueFamilyIndices
  --              , preTransform       = currentTransform
  --                                       (surfaceCaps ∷ SurfaceCapabilitiesKHR)
  --              , compositeAlpha     = COMPOSITE_ALPHA_OPAQUE_BIT_KHR
  --              , presentMode        = presentMode
  --              , clipped            = True
  --              }
  --swapchain ← allocResource (destroyVulkanSwapchain dev)
  --              $ createSwapchainKHR dev swapchainCreateInfo Nothing
  --pure ( pdev, dev, graphicsQueue, gqfamind, presentQueue, SF.format surfaceFormat
  --     , SW.imageExtent swapchainCreateInfo, swapchain )

destroyVulkanDevice ∷ Device → Prog ε σ ()
destroyVulkanDevice dev = destroyDevice dev Nothing
destroyVulkanSwapchain ∷ Device → SwapchainKHR → Prog ε σ ()
destroyVulkanSwapchain dev sc = destroySwapchainKHR dev sc Nothing

-- | Find the device which has the most memory and a graphics queue family index
pickGraphicalPhysicalDevice ∷ Instance → SurfaceKHR → V.Vector BS.ByteString → SurfaceFormatKHR
  → Prog ε σ ( PhysicalDevice, Word32, Word32, SurfaceFormatKHR
             , PresentModeKHR, SurfaceCapabilitiesKHR )
pickGraphicalPhysicalDevice inst surface _requiredExtensions desiredFormat = do
  (_, devs) ← enumeratePhysicalDevices inst
  graphicsDevs    ← fmap (V.mapMaybe id) . for devs $ \dev → runMaybeT $ do
    graphicsQueue ← MaybeT $ headMay <$> getGraphicsQueueIndices dev
    presentQueue  ← MaybeT $ headMay <$> getPresentQueueIndices dev
    guard         =<< deviceHasSwapchain dev
    bestFormat    ← getFormat dev
    presentMode   ← getPresentMode dev
    surfaceCaps   ← getPhysicalDeviceSurfaceCapabilitiesKHR dev surface
    score         ← deviceScore dev
    pure ( score, ( dev, graphicsQueue, presentQueue, bestFormat, presentMode, surfaceCaps) )
  if V.null graphicsDevs then do
    logInfo "no suitable devices found"
    liftIO exitFailure
  else pure . snd . V.maximumBy (comparing fst) $ graphicsDevs
  where headMay = \case
          [] → Nothing
          xs → Just (V.unsafeHead xs)
        deviceScore ∷ MonadIO m ⇒ PhysicalDevice → m (Word64)
        deviceScore dev = do
          heaps ← memoryHeaps <$> getPhysicalDeviceMemoryProperties dev
          features ← getPhysicalDeviceFeatures dev
          let totalSize = sum $ DI.size <$> heaps
              score = if samplerAnisotropy features
                then totalSize
                else 0
          pure score
        deviceHasSwapchain ∷ MonadIO m ⇒ PhysicalDevice → m Bool
        deviceHasSwapchain dev = do
          (_, extensions) ← enumerateDeviceExtensionProperties dev Nothing
          pure $ V.any ((KHR_SWAPCHAIN_EXTENSION_NAME ≡) . extensionName) extensions
        getGraphicsQueueIndices ∷ MonadIO m ⇒ PhysicalDevice → m (V.Vector Word32)
        getGraphicsQueueIndices dev = do
          queueFamilyProperties ← getPhysicalDeviceQueueFamilyProperties dev
          let isGraphicsQueue q =
                (QUEUE_GRAPHICS_BIT .&&. queueFlags q) && (queueCount q > 0)
              graphicsQueueIndices = fromIntegral . fst <$> V.filter
                (isGraphicsQueue . snd)
                (V.indexed queueFamilyProperties)
          pure graphicsQueueIndices
        getPresentQueueIndices ∷ MonadIO m ⇒ PhysicalDevice → m (V.Vector Word32)
        getPresentQueueIndices dev = do
          queueFamilyProperties ← getPhysicalDeviceQueueFamilyProperties dev
          let isPresentQueue i = getPhysicalDeviceSurfaceSupportKHR dev i surface
          V.filterM
            isPresentQueue
            (V.generate (V.length queueFamilyProperties) fromIntegral)
        getFormat ∷ MonadIO m ⇒ PhysicalDevice → m SurfaceFormatKHR
        getFormat dev = do
          (_, formats) ← getPhysicalDeviceSurfaceFormatsKHR dev surface
          pure $ case formats of
            [] → desiredFormat
            [SurfaceFormatKHR FORMAT_UNDEFINED _] → desiredFormat
            _
              | V.any
                (\f →
                  SF.format f ≡ SF.format desiredFormat
                    && SF.colorSpace f ≡ SF.colorSpace desiredFormat
                )
                formats
              → desiredFormat
            _ → V.head formats
        getPresentMode ∷ MonadIO m ⇒ PhysicalDevice → MaybeT m PresentModeKHR
        getPresentMode dev = do
          (_, presentModes) ← getPhysicalDeviceSurfacePresentModesKHR dev surface
          let desiredPresentModes =
                [ PRESENT_MODE_MAILBOX_KHR
                , PRESENT_MODE_FIFO_KHR
                , PRESENT_MODE_IMMEDIATE_KHR ]
          MaybeT
            . pure
            . headMay
            . V.filter (`V.elem` presentModes)
            $ desiredPresentModes

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600
(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)


createRenderP :: Device -> Format -> Prog ε σ RenderPass
createRenderP dev swapchainImageFormat = do
  let
    attachmentDescription :: AttachmentDescription
    attachmentDescription = zero
      { format         = swapchainImageFormat
      , samples        = SAMPLE_COUNT_1_BIT
      , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
      , storeOp        = ATTACHMENT_STORE_OP_STORE
      , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
      , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
      , initialLayout  = IMAGE_LAYOUT_UNDEFINED
      , finalLayout    = IMAGE_LAYOUT_PRESENT_SRC_KHR
      }
    subpass :: SubpassDescription
    subpass = zero
      { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
      , colorAttachments  =
        [ zero { attachment = 0
               , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
               }
        ]
      }
    subpassDependency :: SubpassDependency
    subpassDependency = zero
      { srcSubpass    = SUBPASS_EXTERNAL
      , dstSubpass    = 0
      , srcStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      , srcAccessMask = zero
      , dstStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      , dstAccessMask = ACCESS_COLOR_ATTACHMENT_READ_BIT
                          .|. ACCESS_COLOR_ATTACHMENT_WRITE_BIT
      }
  rp ← allocResource (destroyVulkanRenderPass dev) $ createRenderPass
    dev
    zero { attachments  = [attachmentDescription]
         , subpasses    = [subpass]
         , dependencies = [subpassDependency]
         }
    Nothing
  return rp

destroyVulkanRenderPass ∷ Device → RenderPass → Prog ε σ ()
destroyVulkanRenderPass dev rp = destroyRenderPass dev rp Nothing
