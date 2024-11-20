-- | structures used by the main draw loop to pass around vulkan data
module Vulk.Data where
-- some general data structures are defined
import Prelude()
import UPrelude
import Sign.Var ( TVar )
import qualified Vulk.GLFW as GLFW

-- | possible results of a paracletus evaluation
data VulkResult = VulkSuccess | VulkError | GLFWError deriving (Show, Eq)

-- | the main data for the vulkan loop
data VulkanLoopData = VulkanLoopData
       { windowSizeChanged ∷ TVar Bool
       , framCount         ∷ TVar Int
       , currentSec        ∷ TVar Int
       }
