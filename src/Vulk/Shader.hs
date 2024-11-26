{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Vulk.Shader where
import Prelude()
import UPrelude
import Control.Monad.State.Class ( modify )
import Prog ( Prog(..) )
import Prog.Data ( State(..) )
import qualified Data.Vector as V
import Vulkan.CStruct.Extends
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.Utils.ShaderQQ.GLSL.Glslang ( vert
                                          , frag )

createShaders ∷ Device → Prog ε σ (V.Vector (SomeStruct PipelineShaderStageCreateInfo))
createShaders dev = do
  let fragCode = [frag|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable
        layout(location = 0) in vec3 fragColor;
        layout(location = 0) out vec4 outColor;
        void main() {
            outColor = vec4(fragColor, 1.0);
        }
      |]
      vertCode = [vert|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable
        layout(location = 0) out vec3 fragColor;
        vec2 positions[3] = vec2[](
          vec2(0.0, -0.5),
          vec2(0.5, 0.5),
          vec2(-0.5, 0.5)
        );
        vec3 colors[3] = vec3[](
          vec3(1.0, 1.0, 0.0),
          vec3(0.0, 1.0, 1.0),
          vec3(1.0, 0.0, 1.0)
        );
        void main () {
          gl_Position = vec4(positions[gl_VertexIndex], 0.0, 1.0);
          fragColor = colors[gl_VertexIndex];
        }
      |]
  fragModule ← createShaderModule dev zero { code = fragCode } Nothing
  vertModule ← createShaderModule dev zero { code = vertCode } Nothing
  modify $ \s → s { stVertShader = Just vertModule
                  , stFragShader = Just fragModule }
  let vertShaderStageCreateInfo = zero { stage   = SHADER_STAGE_VERTEX_BIT
                                       , module' = vertModule
                                       , name    = "main" }
      fragShaderStageCreateInfo = zero { stage   = SHADER_STAGE_FRAGMENT_BIT
                                       , module' = fragModule
                                       , name    = "main" }
  pure [SomeStruct vertShaderStageCreateInfo, SomeStruct fragShaderStageCreateInfo]
                                    
