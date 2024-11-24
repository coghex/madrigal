-- | structures used by the main draw loop to pass around vulkan data
module Vulk.Data where
-- some general data structures are defined
import Prelude()
import UPrelude
import Sign.Var ( TVar )
import Data.Word ( Word32 )
import Foreign.Ptr ( Ptr )
import qualified Vulk.GLFW as GLFW
import qualified Vulkan.Core10 as VK

-- | possible results of a paracletus evaluation
data VulkResult = VulkSuccess | VulkError | GLFWError deriving (Show, Eq)

-- | the main data for the vulkan loop
data VulkanLoopData = VulkanLoopData
       { windowSizeChanged ∷ TVar Bool
       , framCount         ∷ TVar Int
       , currentSec        ∷ TVar Int
       }

-- | we are only using one device, so queues are
--   only relevant to pass data around
data DevQueues = DevQueues { graphicsQueue  ∷ VK.Queue
                           , presentQueue   ∷ VK.Queue
                           , qFamIndices    ∷ Ptr Word32
                           , graphicsFamIdx ∷ Word32
                           , presentFamIdx  ∷ Word32
                           } deriving (Eq, Show)

