{-# LANGUAGE StrictData #-}
module Vulk.VulkGLFW where
import Prelude()
import UPrelude
import Control.Concurrent ( threadDelay )
import Control.Monad ( when, unless, forever )
import Control.Monad.State.Class ( gets, modify )
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Data ( FPS(..) )
import Prog ( Prog, Prog', MonadIO(liftIO), MonadReader(ask) )
import Prog.Data ( Env(..), State(..), LoopControl(..) )
import Prog.Util ( allocResource, logInfo, logDebug, logExcept
                 , locally, occupyThreadAndFork )
import Sign.Except ( ExType(ExVulk) )
import Sign.Var ( TVar, writeTVar, atomically )
import Vulk.Callback ( errorCallback, keyCallback )
import Vulk.Data ( VulkResult(..) )
import Vulk.GLFW (WindowHint(..),ClientAPI(..))
import qualified Vulk.GLFW as GLFW


-- | setting of glfw callbacks and hints
initGLFWWindow ∷ Int → Int → String → TVar Bool → Prog ε σ GLFW.Window
initGLFWWindow w h n windowSizeChanged = do
  env ← ask
  let eventQ = envEventQ env
  allocResource
    (\() → liftIO GLFW.terminate ≫ logInfo "glfw terminated")
    (liftIO GLFW.init ⌦ flip unless
      (logExcept GLFWError ExVulk "failed to init glfw") )
  liftIO $ GLFW.setErrorCallback $ Just $ errorCallback eventQ
  liftIO GLFW.getVersionString ⌦ mapM_ (logInfo ∘ ("glfw version: " ⧺))
  liftIO GLFW.vulkanSupported ⌦ flip unless
    (logExcept GLFWError ExVulk "this glfw does not support vulkan")
  liftIO ∘ GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
  liftIO ∘ GLFW.windowHint $ WindowHint'Resizable True
  allocResource
    ( \window → do
        liftIO (GLFW.destroyWindow window)
        logDebug "closed glfw window"
    ) $ do
    mw ← liftIO $ GLFW.createWindow w h n Nothing Nothing
    case mw of
      Nothing → logExcept GLFWError ExVulk "failed to init glfw"
      Just window → do
        logDebug "initialized glfw window"
        liftIO $ GLFW.setKeyCallback window
          $ Just $ keyCallback eventQ
        liftIO $ GLFW.setWindowSizeCallback window
          $ Just (\_ _ _ → do
            atomically $ writeTVar windowSizeChanged True )
        return window

-- | draw loop logic
glfwLoop ∷ GLFW.Window → Prog' ε LoopControl → Prog ε σ Bool
glfwLoop w action = go
  where
    go = do
      should ← liftIO $ GLFW.windowShouldClose w
      if not should then do
        newtick ← liftIO getCurTick
        status  ← locally action
        let fpscap = 60
        FPS fps dfps ← gets stFPS
        let deltafps = 0.1
        liftIO $ whileM_ ((\cur → (cur - newtick) < (1.0/fps))
          <$> getCurTick) (liftIO (threadDelay 1000))
        if dfps > fpscap then modify
             $ \s → s { stFPS = FPS (fps-deltafps) dfps }
        else if dfps < fpscap then modify
          $ \s → s { stFPS = FPS
            (min 200.0 (fps+deltafps)) dfps }
        else modify $ \s → s { stFPS = FPS fps dfps }
        if status ≡ ContinueLoop then go else return False
      else return True

-- | generic monadic loop
whileM_ :: (Monad m) => m Bool -> m () -> m ()
whileM_ p f = do
  x <- p
  when x $ do f >> whileM_ p f

-- | gets time in ms
getCurTick ∷ IO Double
getCurTick = do
  tickUCT ← getCurrentTime
  return (fromIntegral (round
    $ utctDayTime tickUCT * 1000000   ∷ Integer)
                          / 1000000.0 ∷ Double)

-- | runs glfw in the main thread
--   waiting for events every second
glfwWaitEventsMeanwhile ∷ Prog' ε () → Prog ε σ ()
--glfwWaitEventsMeanwhile action = occupyThreadAndFork
--  (liftIO $ forever $ GLFW.waitEventsTimeout 1.0) action
glfwWaitEventsMeanwhile = occupyThreadAndFork
  (liftIO $ forever $ GLFW.waitEventsTimeout 1.0)

-- | glfw will wait when minimized
--   so as not to steal input
glfwWaitMinimized ∷ GLFW.Window → Prog ε σ ()
glfwWaitMinimized win = liftIO go where
  go = do
    (x,y) ← GLFW.getFramebufferSize win
    GLFW.waitEvents
    when (x ≡ 0 ∧ y ≡ 0) go
