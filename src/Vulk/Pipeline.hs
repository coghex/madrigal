{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Vulk.Pipeline where
import Prelude()
import UPrelude
import Control.Monad.State.Class ( modify )
import Data.Bits ( (.|.) )
import Data.Traversable ( for )
import qualified Data.Vector as V
import Prog ( Prog(..) )
import Prog.Data ( State(..) )
import Vulk.Shader ( createShaders )
import Vulkan.CStruct.Extends
import Vulkan.Core10
import Vulkan.Zero

createGraphicsPipeline ∷ Device → RenderPass → Extent2D → Format → Prog ε σ Pipeline
createGraphicsPipeline dev renderPass swapchainExtent _swapchainImageFormat = do
  shaderStages   ← createShaders dev
  pipelineLayout ← createPipelineLayout dev zero Nothing
  modify $ \s → s { stPipelineLayout = Just pipelineLayout }
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
