{-# LANGUAGE Strict #-}
-- | mouse related functionality, inluding scroll,
--   click, and positional monitoring 
module Prog.Mouse where
-- mouse input is handled
import Prelude()
import UPrelude
import Prog ( MonadIO(liftIO), Prog, MonadReader(ask) )
import Prog.Data
import Prog.Util ( logInfo )
import Sign.Queue ( writeQueue )
import Sign.Var ( atomically )
import qualified Vulk.GLFW as GLFW

-- | scroll processing
evalScroll ∷ GLFW.Window → Double → Double → Prog ε σ ()
evalScroll _ _ _ = do
--  env ← ask
  logInfo "TODO: scroll input"

-- | processes mouse on click
evalMouse ∷ GLFW.Window → GLFW.MouseButton
  → GLFW.MouseButtonState → GLFW.ModifierKeys → Prog ε σ ()
evalMouse _   mb mbs mk = do
  env ← ask
  liftIO $ atomically $ writeQueue (envInpQ env) $ InpActMouse mb mbs mk
--  when ((mb ≡ GLFW.mousebutt1)
--         ∧ (not (GLFW.modifierKeysControl mk))) $ do
--    if (mbs ≡ GLFW.MouseButtonState'Pressed) then do
--      pos   ← liftIO $ GLFW.getCursorPos win
--      env   ← ask
--      oldIS ← gets stInput
--      let loadQ = envLoadQ env
--          newIS = oldIS { mouse1 =
--            Just (realToFrac (fst pos), realToFrac (snd pos)) }
--      modify' $ \s → s { stInput = newIS }
--    else if (mbs ≡ GLFW.MouseButtonState'Released) then do
--      oldIS ← gets stInput
--      let newIS = oldIS { mouse1 = Nothing }
--      --env ← ask
--      --pos ← liftIO $ GLFW.getCursorPos win
--      --winsize ← liftIO $ GLFW.getWindowSize win
--      --liftIO $ atomically $ writeQueue (envEventQ env) $ EventLogInfo
--         $ "mouse: " ⧺ (show pos) ⧺ ", normalised: "
--         ⧺ (show (normaliseCoord (pos) (winsize)))
--      modify' $ \s → s { stInput = newIS }
--    else return ()

-- | converts pixel coords into screen coords, right now
--   screen size has no effect on how zoomed in things are
--   so we can ignore it, this may change
normaliseCoord ∷ (Double,Double) → (Int,Int) → (Double,Double)
normaliseCoord (mx,my) (_ ,_ ) = (x,y)
  where x = mx / 64.0
        y = my / 64.0
