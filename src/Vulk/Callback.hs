-- some callbacks that we can register with GLFW
module Vulk.Callback where
-- callbacks for GLFW are defined
import Sign.Data
import Sign.Queue ( Queue, writeQueue )
import Sign.Var ( atomically )
import qualified Vulk.GLFW as GLFW

-- | on error we go straight to the event queue
errorCallback ∷ Queue Event → GLFW.Error → String → IO ()
errorCallback tc e s = atomically $ writeQueue tc $ EventError e s
-- | keys are handed off to the input thread
keyCallback ∷ Queue Event → GLFW.Window → GLFW.Key
  → Int → GLFW.KeyState → GLFW.ModifierKeys → IO ()
keyCallback tc win k sc ka mk
  = atomically $ writeQueue tc $ EventInput $ InputKey win k sc ka mk
-- | mouse buttons are handed off to the input thread
mouseButtonCallback ∷ Queue Event → GLFW.Window → GLFW.MouseButton
  → GLFW.MouseButtonState → GLFW.ModifierKeys → IO ()
mouseButtonCallback tc win mb mbs mk = atomically
  $ writeQueue tc $ EventInput $ InputMouseButton win mb mbs mk
-- | scroll ups and downs are handed off to the input thread
scrollCallback ∷ Queue Event → GLFW.Window → Double → Double → IO ()
scrollCallback tx win x y
  = atomically $ writeQueue tx $ EventInput $ InputMouseScroll win x y
