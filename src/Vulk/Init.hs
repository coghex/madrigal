{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Vulk.Init where
import Prelude()
import UPrelude
import Control.Monad ( (=<<) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import Data.Foldable ( toList, for_ )
import Data.List ( partition )
import qualified Data.Text as T
import Data.Traversable ( for )
import qualified Data.Vector as V
import Foreign.Ptr ( castPtr, nullPtr )
import Prog ( Prog(..) )
import Prog.Foreign
import Prog.Util
import Say
import Vulk.Data ( VulkanWindow(..) )
import Vulk.Device ( createGraphicalDevice )
import Vulk.Foreign
import qualified Vulk.GLFW as GLFW
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_validation_features
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Extensions.VK_KHR_get_physical_device_properties2
import Vulkan.Extensions.VK_KHR_portability_enumeration
import Vulkan.CStruct.Extends
import Vulkan.Core10
import Vulkan.Utils.Debug
import Vulkan.Zero

withVulkanWindow ∷ GLFW.Window → T.Text → Int → Int → Prog ε σ VulkanWindow
withVulkanWindow window name width height = do
  instCI   ← vulkInstanceCreateInfo
  vulkInst ← allocResource destroyVulkanInstance
               $ createInstance instCI Nothing
  debugMsg ← allocResource (destroyVulkanDebugUtilsMessengerEXT vulkInst)
               $ createDebugUtilsMessengerEXT vulkInst
                   debugUtilsMessengerCreateInfo Nothing
  submitDebugUtilsMessageEXT vulkInst
                             DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                             DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                             zero { message = "Vulkan Debugging Started..." }
  surface ← allocResource (destroyVulkanSurface vulkInst)
              $ createSurf vulkInst window
  (dev,graphicsQueue,graphicsQueueFamilyIndex,presentQueue,swapchainFormat,swapchainExtent,swapchain) ← createGraphicalDevice vulkInst surface
  (_, images) ← getSwapchainImagesKHR dev swapchain
  let imageViewCreateInfo i = zero
        { image            = i
        , viewType         = IMAGE_VIEW_TYPE_2D
        , format           = swapchainFormat
        , components       = zero { r = COMPONENT_SWIZZLE_IDENTITY
                                  , g = COMPONENT_SWIZZLE_IDENTITY
                                  , b = COMPONENT_SWIZZLE_IDENTITY
                                  , a = COMPONENT_SWIZZLE_IDENTITY }
        , subresourceRange = zero { aspectMask     = IMAGE_ASPECT_COLOR_BIT
                                  , baseMipLevel   = 0
                                  , levelCount     = 1
                                  , baseArrayLayer = 0
                                  , layerCount     = 1 } }
  imageViews ← for images
    $ \i → allocResource (destroyVulkanImage dev)
             $ createImageView dev (imageViewCreateInfo i) Nothing
  pure $ VulkanWindow window dev surface swapchain swapchainExtent swapchainFormat
                      imageViews graphicsQueue graphicsQueueFamilyIndex presentQueue


createSurf ∷ Instance → GLFW.Window → Prog ε σ SurfaceKHR
createSurf vulkInst window = allocaPeek $ runVk ∘ GLFW.createWindowSurface
                               (castPtr (instanceHandle vulkInst))
                               window nullPtr

destroyVulkanInstance ∷ Instance → Prog ε σ ()
destroyVulkanInstance vi = liftIO $ destroyInstance vi Nothing
destroyVulkanDebugUtilsMessengerEXT ∷ Instance → DebugUtilsMessengerEXT → Prog ε σ ()
destroyVulkanDebugUtilsMessengerEXT inst dm
  = liftIO $ destroyDebugUtilsMessengerEXT inst dm Nothing
destroyVulkanSurface ∷ Instance → SurfaceKHR → Prog ε σ ()
destroyVulkanSurface inst surf = destroySurfaceKHR inst surf Nothing
destroyVulkanImage ∷ Device → ImageView → Prog ε σ ()
destroyVulkanImage dev iv = destroyImageView dev iv Nothing

vulkInstanceCreateInfo ∷ MonadIO m ⇒ m ( InstanceCreateInfo
                                         '[DebugUtilsMessengerCreateInfoEXT
                                         , ValidationFeaturesEXT] )
vulkInstanceCreateInfo = do
  glfwReqExts ← liftIO $ traverse BS.packCString =<< GLFW.getRequiredInstanceExtensions
  let glfwReqExts'  = KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME : glfwReqExts
      glfwReqExts'' = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                      : glfwReqExts' -- macOS
  availableExtensionNames <-
    toList
    .   fmap extensionName
    .   snd
    <$> enumerateInstanceExtensionProperties Nothing
  availableLayerNames <-
    toList . fmap layerName . snd <$> enumerateInstanceLayerProperties

  let requiredLayers     = []
      optionalLayers     = ["VK_LAYER_KHRONOS_validation"]
      requiredExtensions = [EXT_DEBUG_UTILS_EXTENSION_NAME] <> glfwReqExts''
      optionalExtensions = [EXT_VALIDATION_FEATURES_EXTENSION_NAME]
  extensions ← partitionOptReq "extension"
                               availableExtensionNames
                               optionalExtensions
                               requiredExtensions
  layers ← partitionOptReq "layer"
                           availableLayerNames
                           optionalLayers
                           requiredLayers
  let instanceCreateFlags = INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
  pure
    $ zero
        { applicationInfo       = Just zero { applicationName = Just "madrigal"
                                            , apiVersion = API_VERSION_1_0 }
        , enabledLayerNames     = V.fromList layers
        , enabledExtensionNames = V.fromList extensions
        , flags                 = instanceCreateFlags
        }
    ::& debugUtilsMessengerCreateInfo
    :&  ValidationFeaturesEXT [] []
    :&  ()

partitionOptReq ∷ (Show a, Eq a, MonadIO m) ⇒ T.Text → [a] → [a] → [a] → m [a]
partitionOptReq type' available optional required = do
  let (optHave, optMissing) = partition (`elem` available) optional
      (reqHave, reqMissing) = partition (`elem` available) required
      tShow                 = T.pack . show
  for_ optMissing
    $ \n → sayErr $ "Missing optional " <> type' <> ": " <> tShow n
  case reqMissing of
    []  → pure ()
    [x] → sayErr $ "Missing required " <> type' <> ": " <> tShow x
    xs  → sayErr $ "Missing required " <> type' <> "s: " <> tShow xs
  pure (reqHave <> optHave)

debugUtilsMessengerCreateInfo :: DebugUtilsMessengerCreateInfoEXT
debugUtilsMessengerCreateInfo = zero
  { messageSeverity = DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                        .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
  , messageType     = DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                      .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                      .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
  , pfnUserCallback = debugCallbackPtr
  }