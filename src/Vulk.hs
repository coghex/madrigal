{-# LANGUAGE TypeApplications #-}
-- | vulkan-specific draw loop, contains calls to GLFW and
--   runs a simple event processor to make changes to state
module Vulk where
-- the main thread is defined
import Prelude()
import UPrelude
import Control.Concurrent ( forkIO )
import Control.Monad ( forM_, when )
import Control.Monad.State.Class ( gets, modify )
import GHC.Stack ( HasCallStack)
import Data ( Color(Color), FPS(FPS) )
import Prog
    ( MonadIO(liftIO),
      Prog,
      MonadError(catchError),
      MonadReader(ask) )
import Prog.Data
    ( Env(..),
      LoopControl(..),
      State(..) )
import Prog.Util ( getTime, logDebug, logExcept, logInfo, loop )

-- | runVulk contains the initialization and
--   running of the vulkan draw loop
runVulk ∷ HasCallStack ⇒ Prog ε σ ()
runVulk = do
  logInfo "i am madrigal"
