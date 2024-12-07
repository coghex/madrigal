{-# LANGUAGE Strict #-}
-- | vulkan specific draw loop
module Vulk where
import Prelude()
import UPrelude
import Control.Concurrent ( forkIO )
import Control.Monad ( when )
import Control.Monad.State.Class ( gets, modify )
import GHC.Stack ( HasCallStack)
import Data ( FPS(..) )
import Lua ( luaThread )
import Net ( networkThread )
import Net.Data ( NetAction(..), NetServiceType(..) )
import Prog ( Prog, MonadIO(liftIO), MonadReader(ask) )
import Prog.Data ( Env(..), State(..), LoopControl(..) )
import Prog.Event ( processEvents )
import Prog.Input ( inputThread )
import Prog.Util ( getTime, logInfo, loop )
import Sign.Data ( TState(TStart) )
import Sign.Queue ( writeChan, writeQueue )
import Sign.Var ( atomically, newTVar, readTVar, writeTVar, modifyTVar' )
import Vulk.Data ( VulkanLoopData(..), VulkanWindow(..) )
import Vulk.Init ( withVulkanWindow, createRenderP, createGraphicsPipeline )
import Vulk.VulkGLFW ( getCurTick, glfwLoop
                     , glfwWaitEventsMeanwhile, initGLFWWindow )
import qualified Vulk.GLFW as GLFW

runVulk ∷ HasCallStack ⇒ Prog ε σ ()
runVulk = do
  windowSizeChanged ← liftIO $ atomically $ newTVar True
  frameCount        ← liftIO $ atomically $ newTVar @Int 0
  currentSec        ← liftIO $ atomically $ newTVar @Int 0
  window ← initGLFWWindow 800 600 "madrigal" windowSizeChanged
  modify $ \s → s { stWindow = Just window }
  env ← ask
  liftIO $ atomically $ writeTVar (envWindow env) $ Just window
  -- THREADS
  _ ← liftIO $ forkIO $ inputThread env window
  liftIO $ atomically $ writeChan (envInpCh env) TStart
  _ ← liftIO $ forkIO $ networkThread env
  liftIO $ atomically $ writeChan (envNetCh env) TStart
  liftIO $ atomically $ writeQueue (envNetQ env)
    $ NetActionNewService NSCommand
  _ ← liftIO $ forkIO $ luaThread env
  liftIO $ atomically $ writeChan (envLuaCh env) TStart

  -- Vulkan
  VulkanWindow {..} ← withVulkanWindow window "madrigal" 800 600
  renderPass        ← createRenderP vwDevice vwFormat
  graphicsPipe      ← createGraphicsPipeline vwDevice renderPass vwExtent vwFormat

  glfwWaitEventsMeanwhile $ do
    let beforeSwapchainCreation ∷ Prog ε σ ()
        beforeSwapchainCreation =
          liftIO $ atomically $ modifyTVar' windowSizeChanged $ const False
    loop $ do
      firstTick ← liftIO $ getCurTick
      beforeSwapchainCreation
      vulkLoop window
        $ VulkanLoopData windowSizeChanged frameCount currentSec
    logInfo "i am madrigal"

vulkLoop ∷ GLFW.Window → VulkanLoopData → Prog ε σ LoopControl
vulkLoop window (VulkanLoopData windowSizeChanged frameCount currentSec) = do
  sizeChanged ← liftIO $ atomically $ readTVar windowSizeChanged
  shouldExit ← glfwLoop window $ do
    env ← ask
    processEvents
    seconds ← getTime
    cur ← liftIO $ atomically $ readTVar currentSec
    if floor seconds ≠ cur then do
      count ← liftIO $ atomically $ readTVar frameCount
      when (cur ≠ 0) $ do
        FPS fpsTarget _ ← gets stFPS
        modify $ \s → s { stFPS = FPS fpsTarget count }
      liftIO $ do
        atomically $ writeTVar currentSec (floor seconds)
        atomically $ writeTVar frameCount 0
    else liftIO $ atomically $ modifyTVar' frameCount succ
    sizeChanged ← liftIO $ atomically $ readTVar windowSizeChanged
    return $ if sizeChanged then AbortLoop else ContinueLoop
  return $ if shouldExit then AbortLoop else ContinueLoop
