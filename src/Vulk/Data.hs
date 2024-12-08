-- | structures used by the main draw loop to pass around vulkan data
module Vulk.Data where
-- some general data structures are defined
import Prelude()
import UPrelude
import qualified Data.Vector as V
import Data.Word ( Word32 )
import Foreign.Ptr ( Ptr(..) )
import Sign.Var ( TVar )
import qualified Vulk.GLFW as GLFW
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain

-- | possible results of a paracletus evaluation
data VulkResult = VulkSuccess | VulkError | GLFWError deriving (Show, Eq)

-- | the main data for the vulkan loop
data VulkanLoopData = VulkanLoopData
       { windowSizeChanged       ∷ TVar Bool
       , framCount               ∷ TVar Int
       , currentSec              ∷ TVar Int
       , device                  ∷ Device
       , swapchain               ∷ SwapchainKHR
       , graphicsQueue           ∷ Queue
       , presentQueue            ∷ Queue
       , imageAvailableSemaphore ∷ Semaphore
       , renderFinishedSemaphore ∷ Semaphore
       , commandBuffers          ∷ V.Vector CommandBuffer
       }

-- 
data VulkanWindow = VulkanWindow
  { vwGLFWWindow               ∷ GLFW.Window
  , vwDevice                   ∷ Device
  , vwSurface                  ∷ SurfaceKHR
  , vwSwapchain                ∷ SwapchainKHR
  , vwExtent                   ∷ Extent2D
  , vwFormat                   ∷ Format
  , vwImageViews               ∷ V.Vector ImageView
  , vwGraphicsQueue            ∷ Queue
  , vwGraphicsQueueFamilyIndex ∷ Word32
  , vwPresentQueue             ∷ Queue
  }

-- | we are only using one device, so queues are
--   only relevant to pass data around
data DevQueues = DevQueues { graphicsQueue  ∷ Queue
                           , presentQueue   ∷ Queue
                           , qFamIndices    ∷ Ptr Word32
                           , graphicsFamIdx ∷ Word32
                           , presentFamIdx  ∷ Word32
                           } deriving (Eq, Show)
