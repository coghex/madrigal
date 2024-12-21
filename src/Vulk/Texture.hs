module Vulk.Texture where
import Prelude()
import UPrelude
import qualified Codec.Picture as JP
import Control.Monad ( when, filterM )
import qualified Data.Vector as V
import qualified Data.Vector.Storable as Vec
import Data.Word ( Word32(..) )
import Foreign.Marshal.Array ( copyArray, allocaArray )
import Foreign.Ptr ( castPtr )
import Foreign.ForeignPtr ( withForeignPtr )
import Foreign.Storable ( peek )
import Prog ( Prog(..), MonadIO(liftIO) )
import Prog.Util ( logExcept, locally, allocResource, allocResource'
                 , logInfo )
import Sign.Except ( ExType(..) )
import Vulkan.CStruct.Extends ( SomeStruct(..) )
import Vulkan.Core10
import Vulkan.Core10 as Vulk
import Vulkan.Zero
import Vulk.Buff ( createVulkanBuffer, findMemoryType )
import Vulk.Command ( runCommandsOnce )
import Vulk.Data ( VulkResult(..) )
import Vulk.Foreign ( runVk )

createTextureImageView ∷ PhysicalDevice → Device → CommandPool
  → Queue → FilePath → Prog ε σ (ImageView, Word32)
createTextureImageView pdev dev cmdPool cmdQueue path = do
  JP.Image { JP.imageWidth, JP.imageHeight, JP.imageData }
    ← liftIO (JP.readImage path) ⌦ \case
      Left err → logExcept err ExVulk "cannot create texture image view"
      Right dynImg → pure $ JP.convertRGBA8 dynImg
  let (imageDataForeignPtr, imageDataLen)
        = Vec.unsafeToForeignPtr0 imageData
      bufSize ∷ DeviceSize = fromIntegral imageDataLen
      mipLevels = (floor ∘ logBase (2 ∷ Float)
                ∘ fromIntegral $ max imageWidth imageHeight) + 1
  (_, image) ← createVulkanImage pdev dev
    (fromIntegral imageWidth) (fromIntegral imageHeight)
      mipLevels SAMPLE_COUNT_1_BIT FORMAT_R8G8B8A8_UNORM
      IMAGE_TILING_OPTIMAL (IMAGE_USAGE_TRANSFER_SRC_BIT
      ⌄ IMAGE_USAGE_TRANSFER_DST_BIT ⌄ IMAGE_USAGE_SAMPLED_BIT)
      MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  locally $ do
    (stagingMem, stagingBuf) ← createVulkanBuffer pdev dev bufSize
      BUFFER_USAGE_TRANSFER_SRC_BIT
      (MEMORY_PROPERTY_HOST_VISIBLE_BIT
      ⌄ MEMORY_PROPERTY_HOST_COHERENT_BIT )
    stagingDataPtr ← mapMemory dev stagingMem 0 bufSize zero
    liftIO $ withForeignPtr imageDataForeignPtr
      $ \imageDataPtr → copyArray (castPtr stagingDataPtr)
                          imageDataPtr imageDataLen
    unmapMemory dev stagingMem
    runCommandsOnce dev cmdPool cmdQueue $ \cmdBuf → do
      logInfo "starting image layout transitions and copy"
      -- Explicit barrier to transition from UNDEFINED to TRANSFER_DST
      let barrier1 = zero 
            { oldLayout           = IMAGE_LAYOUT_UNDEFINED
            , newLayout           = IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
            , srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED
            , dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED
            , image               = image
            , subresourceRange    = zero 
                { aspectMask     = IMAGE_ASPECT_COLOR_BIT
                , baseMipLevel   = 0
                , levelCount     = mipLevels
                , baseArrayLayer = 0
                , layerCount     = 1 }
            , srcAccessMask       = zero
            , dstAccessMask       = ACCESS_TRANSFER_WRITE_BIT
            }
      cmdPipelineBarrier cmdBuf 
                         PIPELINE_STAGE_TOP_OF_PIPE_BIT
                         PIPELINE_STAGE_TRANSFER_BIT
                         zero V.empty V.empty $ V.fromList [SomeStruct barrier1]
      logInfo "Executed first transition barrier"
      copyBufferToImage cmdBuf stagingBuf image
        (fromIntegral imageWidth) (fromIntegral imageHeight)
      logInfo "Completed image copy"
      transitionImageLayout image FORMAT_R8G8B8A8_UNORM
        TransDst_ShaderRO mipLevels cmdBuf
  imageView ← createVulkanImageView dev image
    FORMAT_R8G8B8A8_UNORM IMAGE_ASPECT_COLOR_BIT mipLevels
  return (imageView, mipLevels)

createVulkanImage ∷ PhysicalDevice → Device → Word32 → Word32 → Word32
  → SampleCountFlagBits → Format → ImageTiling → ImageUsageFlags
  → MemoryPropertyFlags → Prog ε σ (DeviceMemory, Image)
createVulkanImage pdev dev width height mipLevels samples format
                  tiling usage propFlags = do
  let ici = zero { flags              = zero
                 , imageType          = IMAGE_TYPE_2D
                 , extent             = zero { width = width
                                             , height = height
                                             , depth = 1 }
                 , mipLevels          = mipLevels
                 , arrayLayers        = 1
                 , format             = format
                 , tiling             = tiling
                 , initialLayout      = IMAGE_LAYOUT_UNDEFINED
                 , usage              = usage
                 , sharingMode        = SHARING_MODE_EXCLUSIVE
                 , samples            = samples
                 , queueFamilyIndices = V.empty }
  (image, freeImageLater) ← allocResource'
    (\img → destroyImage dev img Nothing)
    $ createImage dev ici Nothing
  MemoryRequirements siz _ mtb ← getImageMemoryRequirements dev image
  memType ← findMemoryType pdev mtb propFlags
  let allocInfo = zero { allocationSize  = siz
                       , memoryTypeIndex = memType }
  imageMemory ← allocResource
    (\iMem → freeMemory dev iMem Nothing)
    $ allocateMemory dev allocInfo Nothing
  freeImageLater
  bindImageMemory dev image imageMemory 0
  return (imageMemory, image)

copyBufferToImage ∷ CommandBuffer → Buffer
  → Image → Word32 → Word32 → Prog ε σ ()
copyBufferToImage cmdBuf buffer image width height
  = cmdCopyBufferToImage cmdBuf buffer image
                           IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL $ V.singleton region
  where region = zero { bufferOffset      = 0
                      , bufferRowLength   = 0
                      , bufferImageHeight = 0
                      , imageSubresource  = zero
                          { aspectMask     = IMAGE_ASPECT_COLOR_BIT
                          , mipLevel       = 0
                          , baseArrayLayer = 0
                          , layerCount     = 1 }
                      , imageOffset       = zero { x=0,y=0,z=0 }
                      , imageExtent       = zero { width  = width
                                                 , height = height
                                                 , depth  = 1 } }

createVulkanImageView ∷ Device → Image → Format
  → ImageAspectFlags → Word32 → Prog ε σ ImageView
createVulkanImageView dev image format aspectFlags mipLevels
  = allocResource (\iv0 → destroyImageView dev iv0 Nothing)
  $ createImageView dev imgvci Nothing
  where cmapping = zero { r = COMPONENT_SWIZZLE_IDENTITY
                        , g = COMPONENT_SWIZZLE_IDENTITY
                        , b = COMPONENT_SWIZZLE_IDENTITY
                        , a = COMPONENT_SWIZZLE_IDENTITY }
        srrange  = zero { aspectMask       = aspectFlags
                        , baseMipLevel     = 0
                        , levelCount       = mipLevels
                        , baseArrayLayer   = 0
                        , layerCount       = 1 }
        imgvci   = zero { flags            = zero
                        , image            = image
                        , viewType         = IMAGE_VIEW_TYPE_2D
                        , format           = format
                        , components       = cmapping
                        , subresourceRange = srrange }

findSupportedFormat ∷ PhysicalDevice → [Format] → ImageTiling
  → FormatFeatureFlags → Prog ε σ Format
findSupportedFormat pdev candidates tiling features = do
  goodCands ← flip filterM candidates $ \format → do
    props ← getPhysicalDeviceFormatProperties pdev format
    return $ case tiling of
      IMAGE_TILING_LINEAR  → linearTilingFeatures  props ⌃ features ≡ features
      IMAGE_TILING_OPTIMAL → optimalTilingFeatures props ⌃ features ≡ features
      _                    → False
  case goodCands of
    x:_ → return x
    []  → logExcept VulkError ExVulk "failed to find supported format"
findDepthFormat ∷ PhysicalDevice → Prog ε σ Format
findDepthFormat pdev = findSupportedFormat pdev
  [FORMAT_D32_SFLOAT, FORMAT_D32_SFLOAT_S8_UINT
  , FORMAT_D24_UNORM_S8_UINT] IMAGE_TILING_OPTIMAL
    FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
hasStencilComponent ∷ Format → Bool
hasStencilComponent format = format `elem`
  [FORMAT_D32_SFLOAT_S8_UINT, FORMAT_D24_UNORM_S8_UINT]

data ImageLayoutTransition = Undef_TransDst
                           | TransDst_ShaderRO
                           | Undef_DepthStencilAtt
                           | Undef_ColorAtt
data TransitionDependent = TransitionDependent
  { oldLayout     ∷ ImageLayout
  , newLayout     ∷ ImageLayout
  , srcAccessMask ∷ AccessFlags
  , dstAccessMask ∷ AccessFlags
  , srcStageMask  ∷ PipelineStageFlags
  , dstStageMask  ∷ PipelineStageFlags }
dependents ∷ ImageLayoutTransition → TransitionDependent
dependents Undef_TransDst = TransitionDependent
  { oldLayout     = IMAGE_LAYOUT_UNDEFINED
  , newLayout     = IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  , srcAccessMask = zero
  , dstAccessMask = ACCESS_TRANSFER_WRITE_BIT
  , srcStageMask  = PIPELINE_STAGE_TOP_OF_PIPE_BIT
  , dstStageMask  = PIPELINE_STAGE_TRANSFER_BIT }
dependents TransDst_ShaderRO = TransitionDependent
  { oldLayout     = IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  , newLayout     = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  , srcAccessMask = ACCESS_TRANSFER_WRITE_BIT
  , dstAccessMask = ACCESS_SHADER_READ_BIT
  , srcStageMask  = PIPELINE_STAGE_TRANSFER_BIT
  , dstStageMask  = PIPELINE_STAGE_FRAGMENT_SHADER_BIT }
dependents Undef_DepthStencilAtt = TransitionDependent
  { oldLayout     = IMAGE_LAYOUT_UNDEFINED
  , newLayout     = IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  , srcAccessMask = zero
  , dstAccessMask = ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
                  ⌄ ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
  , srcStageMask  = PIPELINE_STAGE_TOP_OF_PIPE_BIT
  , dstStageMask  = PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT }
dependents Undef_ColorAtt = TransitionDependent
  { oldLayout     = IMAGE_LAYOUT_UNDEFINED
  , newLayout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  , srcAccessMask = zero
  , dstAccessMask = ACCESS_COLOR_ATTACHMENT_READ_BIT
                  ⌄ ACCESS_COLOR_ATTACHMENT_WRITE_BIT
  , srcStageMask  = PIPELINE_STAGE_TOP_OF_PIPE_BIT
  , dstStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT }

transitionImageLayout ∷ Image → Format → ImageLayoutTransition
  → Word32 → CommandBuffer → Prog ε σ ()
transitionImageLayout image format transition mipLevels cmdBuf = do
  let TransitionDependent{ oldLayout     = ol,  newLayout     = nl
                         , srcAccessMask = sam, dstAccessMask = dam
                         , srcStageMask  = ssm ,dstStageMask  = dsm }
                             = dependents transition
      aspectM = case nl of
        IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
          | hasStencilComponent format → IMAGE_ASPECT_DEPTH_BIT
                                       ⌄ IMAGE_ASPECT_STENCIL_BIT
          | otherwise                  → IMAGE_ASPECT_DEPTH_BIT
        _ → IMAGE_ASPECT_COLOR_BIT
      barrier = zero { oldLayout           = ol
                     , newLayout           = nl
                     , srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED
                     , dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED
                     , image               = image
                     , subresourceRange    = zero { aspectMask     = aspectM
                                                  , baseMipLevel   = 0
                                                  , levelCount     = mipLevels
                                                  , baseArrayLayer = 0
                                                  , layerCount     = 1 }
                     , srcAccessMask       = sam
                     , dstAccessMask       = dam }


  logInfo $ "transitioning image layout from "
          ⧺ show ol ⧺ " to " ⧺ show nl
  cmdPipelineBarrier cmdBuf ssm dsm zero V.empty V.empty
                     $ V.singleton $ SomeStruct barrier
  logInfo "image layout transition complete"
generateMipmaps ∷ PhysicalDevice → Image → Format → Word32
  → Word32 → Word32 → CommandBuffer → Prog ε σ ()
generateMipmaps pdev image format width height mipLevels cmdBuf = do
  formatProps ← getPhysicalDeviceFormatProperties pdev format
  let supported = optimalTilingFeatures formatProps
                ⌃ FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
   in when (supported ≡ zero) $ logExcept VulkError ExVulk
       "texture image format does not support linear blitting"
  mapM_ createLvl
    (zip3
      [1 .. mipLevels - 1]
      (iterate nextLen (fromIntegral width))
      (iterate nextLen (fromIntegral height)))
  let barrier = barrierStruct (mipLevels - 1)
                IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                ACCESS_TRANSFER_WRITE_BIT
                ACCESS_SHADER_READ_BIT
   in cmdPipelineBarrier cmdBuf PIPELINE_STAGE_TRANSFER_BIT
                         PIPELINE_STAGE_FRAGMENT_SHADER_BIT
                         zero V.empty V.empty V.empty
  where
  nextLen l = if l > 1 then l `div` 2 else 1
  barrierStruct mipLevel oldLayout newLayout srcAccessMask dstAccessMask
    = zero { oldLayout           = oldLayout
           , newLayout           = newLayout
           , srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED
           , dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED
           , image               = image
           , subresourceRange    = zero { aspectMask     = IMAGE_ASPECT_COLOR_BIT
                                        , baseMipLevel   = mipLevel
                                        , levelCount     = 1
                                        , baseArrayLayer = 0
                                        , layerCount     = 1 }
           , srcAccessMask       = srcAccessMask
           , dstAccessMask       = dstAccessMask }
  blitStruct mipLevel srcWidth srcHeight = zero
    { srcOffsets = ( zero { x = 0, y = 0, z = 0 } ∷ Offset3D
                   , zero { x = srcWidth
                          , y = srcHeight
                          , z = 1 } ∷ Offset3D )
    , dstOffsets = ( zero { x = 0, y = 0, z = 0 } ∷ Offset3D
                   , zero { x = (nextLen srcWidth)
                          , y = (nextLen srcHeight)
                          , z = 1 } ∷ Offset3D )
    , srcSubresource = zero { aspectMask     = IMAGE_ASPECT_COLOR_BIT
                            , mipLevel       = (mipLevel - 1)
                            , baseArrayLayer = 0
                            , layerCount     = 1 }
    , dstSubresource = zero { aspectMask     = IMAGE_ASPECT_COLOR_BIT
                            , mipLevel       = mipLevel
                            , baseArrayLayer = 0
                            , layerCount     = 1 } }
  createLvl (mipLevel, srcWidth, srcHeight) = do
    let barrier = barrierStruct (mipLevels - 1)
                  IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                  IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                  ACCESS_TRANSFER_WRITE_BIT
                  ACCESS_TRANSFER_READ_BIT
     in cmdPipelineBarrier cmdBuf PIPELINE_STAGE_TRANSFER_BIT
                           PIPELINE_STAGE_TRANSFER_BIT zero
                           V.empty V.empty V.empty
    let blit = V.singleton $ blitStruct mipLevel srcWidth srcHeight
    cmdBlitImage cmdBuf image IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL image
                 IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL blit FILTER_NEAREST
    let barrier = barrierStruct (mipLevels - 1)
                    IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                    IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                    ACCESS_TRANSFER_READ_BIT
                    ACCESS_SHADER_READ_BIT
     in cmdPipelineBarrier cmdBuf PIPELINE_STAGE_TRANSFER_BIT
                           PIPELINE_STAGE_FRAGMENT_SHADER_BIT
                           zero V.empty V.empty V.empty

createTextureSampler ∷ Device → Word32 → Prog ε σ Sampler
createTextureSampler dev mipLevels = allocResource
  (\sampler → destroySampler dev sampler Nothing)
  $ createSampler dev samplerInfo Nothing
  where
    samplerInfo = zero
      { magFilter = FILTER_NEAREST
      , minFilter = FILTER_NEAREST
      , addressModeU = SAMPLER_ADDRESS_MODE_REPEAT
      , addressModeV = SAMPLER_ADDRESS_MODE_REPEAT
      , addressModeW = SAMPLER_ADDRESS_MODE_REPEAT
      , anisotropyEnable = False
      , maxAnisotropy = 1
      , borderColor = BORDER_COLOR_INT_OPAQUE_BLACK
      , unnormalizedCoordinates = False
      , compareEnable = False
      , compareOp = COMPARE_OP_ALWAYS
      , mipmapMode = SAMPLER_MIPMAP_MODE_NEAREST
      , mipLodBias = 0
      , minLod = 0
      , maxLod = 0 -- no mipmapping
      }
createVulkanDescriptorPool ∷ Device → Prog ε σ DescriptorPool
createVulkanDescriptorPool dev = allocResource
  (\pool → destroyDescriptorPool dev pool Nothing)
  $ createDescriptorPool dev poolInfo Nothing
  where
    poolSize = zero
      { type' = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
      , descriptorCount = 1
      }
    poolInfo = zero
      { maxSets = 1
      , poolSizes = V.singleton poolSize
      }
createTextureDescriptorSet ∷ Device → DescriptorPool → DescriptorSetLayout 
  → ImageView → Sampler → Prog ε σ DescriptorSet
createTextureDescriptorSet dev pool layout textureImageView textureSampler = do
  let allocInfo = zero
        { descriptorPool = pool
        , setLayouts = V.singleton layout
        }
  
  descriptorSets ← allocateDescriptorSets dev allocInfo
  
  let imageInfo = zero
        { imageView = textureImageView
        , imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        , sampler = textureSampler
        }
      writeSet = zero
        { dstSet = V.head descriptorSets
        , dstBinding = 0
        , dstArrayElement = 0
        , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , imageInfo = V.singleton imageInfo
        }
  
  updateDescriptorSets dev (V.singleton (SomeStruct writeSet)) V.empty
  return $ V.head descriptorSets
