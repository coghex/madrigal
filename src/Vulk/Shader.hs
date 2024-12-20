{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Vulk.Shader where
import Prelude()
import UPrelude
import Control.Monad.State.Class ( modify )
import Prog ( Prog(..) )
import Prog.Data ( State(..) )
import Prog.Util ( allocResource )
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

        layout(binding = 0) uniform sampler2D texSampler;
        layout(location = 0) in vec3 fragColor;
        layout(location = 1) in vec2 fragTexCoord;
        layout(location = 0) out vec4 outColor;
        
        void main() {
            vec4 texColor = texture(texSampler, fragTexCoord);
            outColor = texColor * vec4(fragColor, 1.0);
        }
      |]
      vertCode = [vert|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable
        
        layout(location = 0) out vec3 fragColor;
        layout(location = 1) out vec2 fragTexCoord;
        
        // Positions and UVs combined for each vertex
        vec2 positions[3] = vec2[](
            vec2(-0.5, -0.5),
            vec2( 0.5, -0.5),
            vec2( 0.0,  0.5)
        );

        vec2 texCoords[3] = vec2[](
            vec2(0.0, 0.0),
            vec2(1.0, 0.0),
            vec2(0.5, 1.0)
        );
        
        vec3 colors[3] = vec3[](
          vec3(1.0, 0.0, 0.0),
          vec3(0.0, 1.0, 0.0),
          vec3(0.0, 0.0, 1.0)
        );
        
        void main() {
            gl_Position = vec4(positions[gl_VertexIndex], 0.0, 1.0);
            fragColor = colors[gl_VertexIndex];
            fragTexCoord = texCoords[gl_VertexIndex];
        }
      |]
  fragModule ← allocResource (\s0 → destroyShaderModule dev s0 Nothing)
                $ createShaderModule dev zero { code = fragCode } Nothing
  vertModule ← allocResource (\s0 → destroyShaderModule dev s0 Nothing)
                $ createShaderModule dev zero { code = vertCode } Nothing
  let vertShaderStageCreateInfo = zero { stage   = SHADER_STAGE_VERTEX_BIT
                                      , module' = vertModule
                                      , name    = "main" }
      fragShaderStageCreateInfo = zero { stage   = SHADER_STAGE_FRAGMENT_BIT
                                      , module' = fragModule
                                      , name    = "main" }
  pure [SomeStruct vertShaderStageCreateInfo, SomeStruct fragShaderStageCreateInfo]
