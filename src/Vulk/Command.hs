{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Vulk.Command where
import Prelude()
import UPrelude
import Control.Monad.State.Class ( modify )
import Data.Traversable ( for )
import qualified Data.Vector as V
import Data.Word ( Word32(..) )
import Prog ( Prog(..), MonadIO(liftIO) )
import Prog.Data ( State(..) )
import Vulkan.Core10
import Vulkan.Zero

createCommandBuffers ∷ Device → RenderPass → Pipeline → Word32
  → V.Vector Framebuffer → Extent2D → Prog ε σ (V.Vector CommandBuffer)
createCommandBuffers dev renderPass graphicsPipeline graphicsQueueFamilyIndex
                     framebuffers swapchainExtent = do
  let commandPoolCreateInfo ∷ CommandPoolCreateInfo
      commandPoolCreateInfo =
        zero { queueFamilyIndex = graphicsQueueFamilyIndex }
  commandPool ← createCommandPool dev commandPoolCreateInfo Nothing
  let commandBufferAllocateInfo ∷ CommandBufferAllocateInfo
      commandBufferAllocateInfo = zero
        { commandPool        = commandPool
        , level              = COMMAND_BUFFER_LEVEL_PRIMARY
        , commandBufferCount = fromIntegral $ V.length framebuffers }
  buffers ← allocateCommandBuffers dev commandBufferAllocateInfo
  _ ← liftIO . for (V.zip framebuffers buffers)
        $ \(framebuffer, buffer) → useCommandBuffer buffer
            zero { flags = COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT }
          $ do
              let renderPassBeginInfo = zero
                    { renderPass  = renderPass
                    , framebuffer = framebuffer
                    , renderArea  = Rect2D { offset = zero
                                           , extent = swapchainExtent }
                    , clearValues = [Color (Float32 0.1 0.1 0.1 0)] }
              cmdUseRenderPass buffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE
                $ do
                    cmdBindPipeline buffer PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
                    cmdDraw buffer 3 1 0 0
  modify $ \s → s { stCmdBuffInfo = Just commandBufferAllocateInfo
                  , stCmdBuffers  = Just buffers }
  pure buffers
