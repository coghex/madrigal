{-# LANGUAGE Strict #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- | vulkan instance
module Vulk.Instance where
import Prelude()
import UPrelude
import Control.Exception ( bracket )
import Control.Monad ( when, (=<<) )
import Control.Monad.State.Class ( gets, modify )
import Control.Monad.IO.Class
import qualified Data.Vector                       as V
import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import Data.List ( partition )
import Data.Foldable ( toList, for_ )
import qualified Data.Text                         as T
import Data.Text.Encoding
import Data.Traversable ( for )
import Data.Word ( Word32 )
import Foreign.Ptr ( castPtr, nullPtr )
import Say
import Prog ( Prog, MonadIO(liftIO), MonadReader(ask), MonadState(get) )
import Prog.Data ( Env(..), State(..), LoopControl(..) )
import Prog.Util ( logInfo, allocResource )
import Prog.Foreign ( allocaPeek )
import Vulk.Device ( createGraphicalDevice )
import Vulk.Foreign
import qualified Vulk.GLFW as GLFW
import           Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_validation_features
import Vulkan.Extensions.VK_KHR_get_physical_device_properties2
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Extensions.VK_KHR_portability_enumeration
import Vulkan.CStruct.Extends
import Vulkan.Core10
import qualified Vulkan.Core10.DeviceInitialization as DI
import Vulkan.Zero

data VulkanWindow = VulkanWindow
  { vwGLFWWindow               :: GLFW.Window
  , vwDevice                   :: Device
  , vwSurface                  :: SurfaceKHR
  , vwSwapchain                :: SwapchainKHR
  , vwExtent                   :: Extent2D
  , vwFormat                   :: Format
  , vwImageViews               :: V.Vector ImageView
  , vwGraphicsQueue            :: Queue
  , vwGraphicsQueueFamilyIndex :: Word32
  , vwPresentQueue             :: Queue
  }

createSurf ∷ Instance → GLFW.Window → Prog ε σ SurfaceKHR
createSurf vulkInst window = allocaPeek $ runVk ∘ GLFW.createWindowSurface
                               (castPtr (instanceHandle vulkInst))
                               window nullPtr

withVulkWindow ∷ GLFW.Window → T.Text → Int → Int → Prog ε σ VulkanWindow
withVulkWindow window name width height = do
  instCI   ← vulkInstanceCreateInfo
  vulkInst ← createInstance instCI Nothing
  debugmsg ← createDebugUtilsMessengerEXT vulkInst debugUtilsMessengerCreateInfo
                                        Nothing
  submitDebugUtilsMessageEXT vulkInst
                             DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                             DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                             zero { message = "Vulkan Debugging Started..." }
  surface ← createSurf vulkInst window
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
    $ \i → createImageView dev (imageViewCreateInfo i) Nothing
  modify $ \s → s { stInstance  = Just vulkInst
                  , stDebugMsg  = Just debugmsg
                  , stSurface   = Just surface
                  , stDevice    = Just dev
                  , stSwapchain = Just swapchain
                  , stImgViews  = Just imageViews }
  pure $ VulkanWindow window dev surface swapchain swapchainExtent swapchainFormat
                      imageViews graphicsQueue graphicsQueueFamilyIndex presentQueue

destroyVulkanInstance ∷ Prog ε σ ()
destroyVulkanInstance = do
  logInfo "cleaning up..."
  State {..} ← get
  case stInstance of
    Nothing → return ()
    Just i0 → do
      destroyMaybe stDebugMsg $ (flip (destroyDebugUtilsMessengerEXT i0)) Nothing
      case stDevice of
        Nothing → return ()
        Just d0 → do
          destroyMaybe stPipeline       $ (flip (destroyPipeline       d0)) Nothing
          destroyMaybe stPipelineLayout $ (flip (destroyPipelineLayout d0)) Nothing
          destroyMaybe stFragShader     $ (flip (destroyShaderModule   d0)) Nothing
          destroyMaybe stVertShader     $ (flip (destroyShaderModule   d0)) Nothing
          destroyMaybe stRenderPass     $ (flip (destroyRenderPass     d0)) Nothing
          case stImgViews of
            Nothing   → return ()
            Just ivs0 → destroyImageViews ivs0
              where destroyImageViews ∷ V.Vector ImageView → Prog ε σ ()
                    destroyImageViews ivs
                      | V.null ivs = return ()
                      | otherwise  = do
                          destroyImageView d0 (V.head ivs) Nothing
                          destroyImageViews $ V.tail ivs

          destroyMaybe stSwapchain    $ (flip (destroySwapchainKHR d0)) Nothing
          destroyDevice d0 Nothing
      destroyMaybe stSurface $ (flip (destroySurfaceKHR i0)) Nothing
      destroyInstance i0 Nothing
destroyMaybe ∷ Maybe α → (α → Prog ε σ ()) → Prog ε σ ()
destroyMaybe Nothing  _ = return ()
destroyMaybe (Just a) f = f a

vulkInstanceCreateInfo ∷ MonadIO m ⇒ m ( InstanceCreateInfo
                                         '[DebugUtilsMessengerCreateInfoEXT
                                         , ValidationFeaturesEXT] )
vulkInstanceCreateInfo = do
  glfwReqExts ← liftIO $ traverse BS.packCString =<< GLFW.getRequiredInstanceExtensions
  let glfwReqExts'  = KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME : glfwReqExts
      glfwReqExts'' = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME : glfwReqExts'
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

partitionOptReq
  :: (Show a, Eq a, MonadIO m) => T.Text -> [a] -> [a] -> [a] -> m [a]
partitionOptReq type' available optional required = do
  let (optHave, optMissing) = partition (`elem` available) optional
      (reqHave, reqMissing) = partition (`elem` available) required
      tShow                 = T.pack . show
  for_ optMissing
    $ \n -> sayErr $ "Missing optional " <> type' <> ": " <> tShow n
  case reqMissing of
    []  -> pure ()
    [x] -> sayErr $ "Missing required " <> type' <> ": " <> tShow x
    xs  -> sayErr $ "Missing required " <> type' <> "s: " <> tShow xs
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
-- | A debug callback which prints the message prefixed with "Validation: " to
-- stderr.
foreign import ccall unsafe "DebugCallback.c &debugCallback"
  debugCallbackPtr :: PFN_vkDebugUtilsMessengerCallbackEXT
-- | A debug callback the same as 'debugCallbackPtr' except it will call
-- @abort@ when @VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT@ is set.
foreign import ccall unsafe "DebugCallback.c &debugCallbackFatal"
  debugCallbackFatalPtr :: PFN_vkDebugUtilsMessengerCallbackEXT

-- | Assign a name to a handle using 'setDebugUtilsObjectNameEXT', note that
-- the @VK_EXT_debug_utils@ extension must be enabled.
nameObject :: (HasObjectType a, MonadIO m) => Device -> a -> BS.ByteString -> m ()
nameObject device object name = setDebugUtilsObjectNameEXT
  device
  (uncurry DebugUtilsObjectNameInfoEXT (objectTypeAndHandle object) (Just name))
