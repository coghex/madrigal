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
import qualified Data.Vector as V
import Lua ( luaThread )
import Net ( networkThread )
import Net.Data ( NetAction(..), NetServiceType(..) )
import Prog ( Prog, MonadIO(liftIO), MonadReader(ask) )
import Prog.Data ( Env(..), State(..), LoopControl(..) )
import Prog.Event ( processEvents )
import Prog.Input ( inputThread )
import Prog.Util ( getTime, logInfo, loop, allocResource )
import Sign.Data ( TState(TStart) )
import Sign.Queue ( writeChan, writeQueue )
import Sign.Var ( atomically, newTVar, readTVar, writeTVar, modifyTVar' )
import Vulk.Command ( createVulkanCommandPool )
import Vulk.Data ( VulkanLoopData(..), VulkanWindow(..), DevQueues(..)
                 , SwapchainInfo(..) )
import Vulk.Init ( withVulkanWindow, createRenderP, createVulkanDescriptorPool
                 , createFramebuffers, createCommandBuffers, createSemaphores
                 , createVulkanGraphicsPipeline )
import Vulk.Loop ( acquireVulkanImage, submitQueue, waitIdle, createVulkanSwapchain
                 , createSwapchainImageViews, createDescriptorSets )
import Vulk.Texture ( createTextureImageView, createTextureSampler )
import Vulk.VulkGLFW ( getCurTick, glfwLoop
                     , glfwWaitEventsMeanwhile, initGLFWWindow )
import qualified Vulk.GLFW as GLFW
import Vulkan.Core10
import Vulkan.Zero ( zero )

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
  (imageAvailableSemaphore, renderFinishedSemaphore) ← createSemaphores vwDevice
  commandPool       ← createVulkanCommandPool vwDevice
                       $ graphicsFamIdx vwDevQueues
    -- Update pipeline layout to include texture sampler
  let layoutBindings = [ zero { binding           = 0
                              , descriptorType
                                  = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                              , descriptorCount   = 1
                              , stageFlags        = SHADER_STAGE_FRAGMENT_BIT } ]
      layoutInfo = zero { bindings = V.fromList layoutBindings }
  descriptorSetLayout ← allocResource
    (\dsl → destroyDescriptorSetLayout vwDevice dsl Nothing)
    $ createDescriptorSetLayout vwDevice layoutInfo Nothing

  (textureView, miplevels)
    ← createTextureImageView vwPhysicalDevice vwDevice commandPool
                             (graphicsQueue vwDevQueues) "dat/tile01.png"
  textureSampler    ← createTextureSampler vwDevice miplevels

  descriptorPool    ← createVulkanDescriptorPool vwDevice
  descriptorSet     ← createDescriptorSets vwDevice descriptorPool
                        descriptorSetLayout textureView textureSampler

  let vwFormat = swapImgFormat vwSwapchain
      vwExtent = swapExtent    vwSwapchain
  renderPass        ← createRenderP vwDevice vwFormat
  (graphicsPipe,pl) ← createVulkanGraphicsPipeline vwDevice renderPass 
                        (swapExtent vwSwapchain) vwFormat descriptorSetLayout
  swapchainImgViews ← createSwapchainImageViews vwDevice vwSwapchain
  framebuffers      ← createFramebuffers vwDevice swapchainImgViews
                        renderPass (swapExtent vwSwapchain)
  commandBuffers    ← createCommandBuffers vwDevice renderPass graphicsPipe pl
                                           (graphicsFamIdx vwDevQueues)
                                           framebuffers (swapExtent vwSwapchain)
                                           descriptorSet
  liftIO $ GLFW.showWindow vwGLFWWindow

  glfwWaitEventsMeanwhile $ do
    let beforeSwapchainCreation ∷ Prog ε σ ()
        beforeSwapchainCreation =
          liftIO $ atomically $ modifyTVar' windowSizeChanged $ const False
    loop $ do
      firstTick ← liftIO $ getCurTick
      beforeSwapchainCreation
      vulkLoop window
        $ VulkanLoopData windowSizeChanged vwDevQueues frameCount
                         currentSec vwPhysicalDevice vwDevice vwSurface
                         imageAvailableSemaphore renderFinishedSemaphore
    logInfo "i am madrigal"

vulkLoop ∷ GLFW.Window → VulkanLoopData → Prog ε σ LoopControl
vulkLoop window (VulkanLoopData windowSizeChanged devQueues frameCount
                                currentSec pdev dev surf imageAvailableSemaphore
                                renderFinishedSemaphore) = do
  sizeChanged ← liftIO $ atomically $ readTVar windowSizeChanged
  shouldExit ← glfwLoop window $ do
    env ← ask
    --imageIndex ← acquireVulkanImage dev swapchain maxBound imageAvailableSemaphore
    --submitQueue swapchain imageAvailableSemaphore renderFinishedSemaphore
    --            imageIndex graphicsQueue presentQueue commandBuffers
    --waitIdle dev graphicsQueue

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
