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
import Vulk.Shader ( createShaders )
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
destroyVulkanRenderPass ∷ Device → RenderPass → Prog ε σ ()
destroyVulkanRenderPass dev rp = destroyRenderPass dev rp Nothing
destroyVulkanPipelineLayout ∷ Device → PipelineLayout → Prog ε σ ()
destroyVulkanPipelineLayout dev pl = destroyPipelineLayout dev pl Nothing

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
  rp ← allocResource (destroyVulkanRenderPass dev)
         $ createRenderPass
    dev
    zero { attachments  = [attachmentDescription]
         , subpasses    = [subpass]
         , dependencies = [subpassDependency]
         }
    Nothing
  return rp

createGraphicsPipeline ∷ Device → RenderPass → Extent2D → Format → Prog ε σ Pipeline
createGraphicsPipeline dev renderPass swapchainExtent _swapchainImageFormat = do
  shaderStages   ← createShaders dev
  pipelineLayout ← allocResource (destroyVulkanPipelineLayout dev)
                     $ createPipelineLayout dev zero Nothing
  let
    Extent2D { width = swapchainWidth, height = swapchainHeight } = swapchainExtent
    pipelineCreateInfo ∷ GraphicsPipelineCreateInfo '[]
    pipelineCreateInfo = zero
      { stages             = shaderStages
      , vertexInputState   = Just zero
      , inputAssemblyState = Just zero
                               { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
                               , primitiveRestartEnable = False }
      , viewportState      = Just . SomeStruct $ zero
        { viewports =
          [ Viewport
            { x        = 0
            , y        = 0
            , width    = realToFrac swapchainWidth
            , height   = realToFrac swapchainHeight
            , minDepth = 0
            , maxDepth = 1 } ]
        , scissors = [Rect2D { offset = Offset2D 0 0, extent = swapchainExtent }] }
      , rasterizationState = Just . SomeStruct $ zero
                               { depthClampEnable        = False
                               , rasterizerDiscardEnable = False
                               , lineWidth               = 1
                               , polygonMode             = POLYGON_MODE_FILL
                               , cullMode                = CULL_MODE_NONE
                               , frontFace               = FRONT_FACE_CLOCKWISE
                               , depthBiasEnable         = False }
      , multisampleState   = Just . SomeStruct $ zero
                               { sampleShadingEnable  = False
                               , rasterizationSamples = SAMPLE_COUNT_1_BIT
                               , minSampleShading     = 1
                               , sampleMask           = [maxBound] }
      , depthStencilState  = Nothing
      , colorBlendState    = Just . SomeStruct $ zero
                               { logicOpEnable = False
                               , attachments   = [ zero
                                                     { colorWriteMask =
                                                       COLOR_COMPONENT_R_BIT
                                                       .|. COLOR_COMPONENT_G_BIT
                                                       .|. COLOR_COMPONENT_B_BIT
                                                       .|. COLOR_COMPONENT_A_BIT
                                                      , blendEnable   = False } ] }
      , dynamicState       = Nothing
      , layout             = pipelineLayout
      , renderPass         = renderPass
      , subpass            = 0
      , basePipelineHandle = zero }
  V.head . snd
    <$> createGraphicsPipelines dev zero [SomeStruct pipelineCreateInfo] Nothing

createFramebuffers ∷ Device → V.Vector ImageView → RenderPass
  → Extent2D → Prog ε σ (V.Vector Framebuffer)
createFramebuffers dev imageViews renderPass Extent2D {width, height} =
  for imageViews $ \imageView → do
    let framebufferCreateInfo ∷ FramebufferCreateInfo '[]
        framebufferCreateInfo = zero
          { renderPass  = renderPass
          , attachments = [imageView]
          , width
          , height
          , layers      = 1 }
    createFramebuffer dev framebufferCreateInfo Nothing
