module Vulk.Buff where
import UPrelude
import Prelude()
import Data.Bits ( testBit )
import Data.Word ( Word32(..) )
import qualified Data.Vector as V
import Prog ( Prog(..) )
import Prog.Util ( allocResource, allocResource', locally, logExcept )
import Sign.Except ( ExType(..) )
import Vulk.Data ( VulkResult(..) )
import Vulk.Command ( runCommandsOnce )
import Vulkan.Core10
import Vulkan.Zero

createVulkanBuffer ∷ PhysicalDevice → Device → DeviceSize
  → BufferUsageFlags → MemoryPropertyFlags → Prog ε σ (DeviceMemory, Buffer)
createVulkanBuffer pdev dev bSize bUsage bMemPropFlags = do
  let bufferInfo = zero { size               = bSize
                        , usage              = bUsage
                        , sharingMode        = SHARING_MODE_EXCLUSIVE
                        , queueFamilyIndices = V.empty }
  (buf, freeBufLater) ← allocResource' (\vb → destroyBuffer dev vb Nothing)
    $ createBuffer dev bufferInfo Nothing
  MemoryRequirements siz _ mtb ← getBufferMemoryRequirements dev buf
  memIndex ← findMemoryType pdev mtb bMemPropFlags
  let allocInfo = zero { allocationSize  = siz
                       , memoryTypeIndex = memIndex }
  bufferMemory ← allocResource (\vbm → freeMemory dev vbm Nothing)
    $ allocateMemory dev allocInfo Nothing
  freeBufLater
  bindBufferMemory dev buf bufferMemory 0
  return (bufferMemory, buf)

copyBuffer ∷ Device → CommandPool → Queue → Buffer
  → Buffer → DeviceSize → Prog ε σ ()
copyBuffer dev cmdPool cmdQueue srcBuffer dstBuffer bSize
  = runCommandsOnce dev cmdPool cmdQueue $ \cmdBuf → do
    let copyRegion = zero { srcOffset = 0
                          , dstOffset = 0
                          , size      = bSize }
    cmdCopyBuffer cmdBuf srcBuffer dstBuffer $ V.singleton copyRegion

findMemoryType ∷ PhysicalDevice → Word32 → MemoryPropertyFlags → Prog ε σ Word32
findMemoryType pdev typeFilter properties = do
  PhysicalDeviceMemoryProperties mtCount memTypes _ _
    ← getPhysicalDeviceMemoryProperties pdev
  let go i | i ≡ mtCount = logExcept VulkError ExVulk
                             "failed to find suitable memory type"
           | otherwise = if testBit typeFilter (fromIntegral i)
                              ∧ (propertyFlags (memTypes V.! (fromIntegral i))
                                ⌃ properties) ≡ properties
                                then return i else go (i+1)
  go 0
