{-# LANGUAGE Strict #-}
-- | some utility functions for GLFW
-- TODO: these functions should be in better places
module Vulk.VulkGLFW where
-- the input side of GLFW is handled,
-- loops and control functions are defined
import Prelude()
import UPrelude
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless, forever)
import Control.Monad.State.Class (gets, modify)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Data ( FPS(..) )
import Prog
    ( MonadIO(liftIO),
      MonadReader(ask),
      Prog,
      Prog' )
import Prog.Data
import Prog.Util
    ( allocResource,
      locally,
      logDebug,
      logExcept,
      logInfo,
      logError,
      occupyThreadAndFork )
import Sign.Except ( ExType(ExVulk) )
import Sign.Var ( atomically, writeTVar, TVar )
import Sign.Queue ( writeQueue )
import Vulk.Data
    ( VulkResult(GLFWError) )
import Vulk.Callback
import Vulk.GLFW (WindowHint(..),ClientAPI(..))
import qualified Vulk.GLFW as GLFW

-- | setting of glfw callbacks and hints
initGLFWWindow ∷ Int → Int → String → TVar Bool → Prog ε σ GLFW.Window
initGLFWWindow w h n windowSizeChanged = do
  env ← ask
  let eventQ = envEventQ env
  allocResource
    (\() → liftIO GLFW.terminate ≫ logInfo "terminated glfw")
    (liftIO GLFW.init ⌦ flip unless
      (logExcept GLFWError ExVulk "failed to init glfw") )
  -- this one we set before we create the window
  liftIO $ GLFW.setErrorCallback $ Just $ errorCallback eventQ
  liftIO GLFW.getVersionString ⌦ mapM_ (logInfo ∘ ("glfw version: " ⧺))
  liftIO GLFW.vulkanSupported ⌦ flip unless
    (logExcept GLFWError ExVulk "glfw does not support vulkan")
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
        liftIO $ GLFW.setKeyCallback         window
          $ Just $ keyCallback         eventQ
        liftIO $ GLFW.setMouseButtonCallback window
          $ Just $ mouseButtonCallback eventQ
        liftIO $ GLFW.setScrollCallback      window
          $ Just $ scrollCallback      eventQ
        liftIO $ GLFW.setWindowSizeCallback  window
          $ Just (\_ _ _ → do
            atomically $ writeTVar windowSizeChanged True )
        return window

-- | loadLoop is the outer loop
loadLoop ∷ GLFW.Window → Prog' ε LoopControl → Prog ε σ Bool
loadLoop w action = go
  where go = do
          should ← liftIO $ GLFW.windowShouldClose w
          if not should then do
            status ← locally action
            if status ≡ ContinueLoop then go else return False
          else return True

-- | this is the inner draw loop, every tick will check the fps
glfwMainLoop ∷ GLFW.Window → Prog' ε LoopControl → Prog ε σ Bool
glfwMainLoop w action = go
  where go = do
          should ← liftIO $ GLFW.windowShouldClose w
          if not should then do
            newtick ← liftIO getCurTick
            status ← locally action
            reload ← gets stReload
            let fpscap = 60
            case reload of
              RSNULL → do
                -- TODO: PID loop
                FPS fps dfps disp ← gets stFPS
                let deltafps = 0.1
                liftIO $ whileM_ ((\cur → (cur - newtick) < (1.0/fps))
                  <$> getCurTick) (liftIO (threadDelay 1000))
                if dfps > fpscap then modify
                     $ \s → s { stFPS = FPS (fps-deltafps) dfps disp }
                else if dfps < fpscap then modify
                  $ \s → s { stFPS = FPS
                    (min 200.0 (fps+deltafps)) dfps disp }
                else modify $ \s → s { stFPS = FPS fps dfps disp }
              _ → return ()
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

-- | this wraps around the draw loop to close the program when we ask
drawLoop ∷ GLFW.Window → Prog' ε Bool → Prog ε σ Bool
drawLoop w action = go
  where go = do
          should ← liftIO $ GLFW.windowShouldClose w
          if not should then do
            status ← locally action
            if status ≡ True then go else return False
          else return True

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

-- | run every frame for handling input
-- TODO: delete this
processInput ∷ Prog ε σ ()
processInput = do
 -- st ← get
--  let inp = stInput st
  --processMousePos inp
  return ()

-- some utility functions to help GLFW
-- | makes glfw fullscreen in prog context
makeFullscreen ∷ Prog ε σ ()
makeFullscreen = do
  win ← gets stWindow
  env ← ask
  logInfo "making fullscreen"
  case win of
    Nothing → logError "no glfw window present"
    Just w0 → do
      m' ← liftIO GLFW.getPrimaryMonitor
      case m' of
        Nothing → logError "no primary monitor present"
        Just m0 → do
          vm' ← liftIO $ GLFW.getVideoMode m0
          case vm' of
            Nothing → logError "no video mode present"
            Just _  →  do
              (_,_,w,h) ← liftIO $ GLFW.getMonitorWorkarea m0
              liftIO $ GLFW.setWindowSize w0 w h
      
-- | makes glfw windowed in prog context
makeWindowed ∷ Int → Int → Int → Int → Prog ε σ ()
makeWindowed w h x y = do
  win ← gets stWindow
  env ← ask
  logInfo "making windowed"
  case win of
    Nothing → logError "no glfw window present"
    Just w0 → do
      liftIO $ GLFW.setWindowed w0 w h x y
