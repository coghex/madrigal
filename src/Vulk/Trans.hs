module Vulk.Trans where
import Prelude()
import UPrelude
import Foreign.Ptr ( castPtr, plusPtr )
import Foreign.Storable ( sizeOf, Storable(..) )
import qualified Foreign.Storable as FS
import Foreign.Marshal.Array ( peekArray, pokeArray )
import GHC.Generics ( Generic )
import qualified Data.Vector.Unboxed as V
import Data.Word ( Word32 )
import Prog ( Prog(..) )
import Vulk.Buff ( createVulkanBuffer )
import Vulkan.Core10
import Vulkan.Zero


