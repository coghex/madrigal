-- | vulkan specific draw loop
module Vulk where
import Prelude()
import UPrelude
import GHC.Stack ( HasCallStack)
import Prog ( Prog )
import Prog.Util ( logInfo )

runVulk ∷ HasCallStack ⇒ Prog ε σ ()
runVulk = do
  logInfo "i am madrigal"
