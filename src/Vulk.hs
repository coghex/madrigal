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
import Prog.Event ( processEvents )
import Prog.Input ( inputThread )
import Prog.Util ( getTime, logDebug, logExcept, logInfo, loop )
import Sign.Data ( LogLevel(..), TState(..) )
import Sign.Except ( ExType(..) )
import Sign.Var
import Sign.Queue ( writeChan )
import Vulk.Data ( VulkResult(..) )
import Vulk.VulkGLFW
import qualified Vulk.GLFW as GLFW

-- | runVulk contains the initialization and
--   running of the vulkan draw loop
runVulk ∷ HasCallStack ⇒ Prog ε σ ()
runVulk = do
    logInfo "beginning madrigal..."
    -- windowsizechanged is completely seperate from all other data
    windowSizeChanged ← liftIO $ atomically $ newTVar True
    window ← initGLFWWindow 800 600 "madrigal" windowSizeChanged
    modify $ \s → s { stWindow = Just window }
    glfwWaitEventsMeanwhile $ do
        -- thread creation
        env ← ask
        _ ← liftIO $ forkIO $ inputThread env window
        liftIO $ atomically $ writeChan (envInpCh env) TStart
        let beforeSwapchainCreation ∷ Prog ε σ ()
            beforeSwapchainCreation =
              liftIO $ atomically $ modifyTVar' windowSizeChanged
              $ const False
        -- the loop function passes around the LoopControl structure
        loop $ do
            logDebug "loading swapchain..."
            beforeSwapchainCreation
            vulkLoop

-- | this is the main draw loop itself
vulkLoop ∷ Prog ε σ LoopControl
vulkLoop = do
  window ← gets stWindow
  case window of
    Nothing → do
                logExcept VulkError ExVulk "no window"
                return AbortLoop
    Just w0 → do
                shouldExit ← glfwMainLoop w0 $ do
                               processEvents
                               return ContinueLoop
                return $ if shouldExit then AbortLoop else ContinueLoop
