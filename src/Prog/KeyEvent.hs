{-# LANGUAGE Strict #-}
-- 
module Prog.KeyEvent where
-- key input is handled
import Prelude()
import UPrelude
import Control.Monad.State.Class (modify)
import qualified Data.Map as Map
import Data ( Key(..), KeyFunc(..), KeyMap(..) )
import Data.Maybe ( fromMaybe )
import Prog
    ( MonadIO(liftIO), Prog, MonadReader(ask), MonadState(get) )
import Prog.Data
import Prog.Util
    ( logDebug )
import Sign.Data
    ( InputStateChange(..) )
import Sign.Queue ( writeQueue )
import Sign.Var ( atomically )
import qualified Vulk.GLFW as GLFW

-- | this used to be the whole input function, now that
--   it is threaded it is just a callback...
evalKey ∷ GLFW.Window → GLFW.Key → GLFW.KeyState
  → GLFW.ModifierKeys → Prog ε σ ()
evalKey _      k ks mk = do
  env ← ask
--  st  ← get
--  let keyLayout = sKeyLayout $ stSettings st
--      cap       = inpCap $ stInput st

  liftIO $ atomically $ writeQueue (envInpQ env) $ InpActKey k ks mk

-- updates game state with input state
updateInputState ∷ InputStateChange → Prog ε σ ()
updateInputState (ISCKeyPress   kf) = do
  st ← get
  let newIS = stateKeyPress kf $ stInput st
  modify $ \s → s { stInput = newIS }
updateInputState (ISCKeyRelease kf) = do
  st ← get
  let newIS = stateKeyRelease kf $ stInput st
  modify $ \s → s { stInput = newIS }
updateInputState (ISCAccelerate ac) = do
  st ← get
  let newIS = stateAccelerate ac $ stInput st
  modify $ \s → s { stInput = newIS }
updateInputState ISCResetCam = do
  st ← get
  let oldIS = stInput st
      newIS = oldIS { keySt = newKS }
      newKS = (keySt oldIS) { keyAccel = (0,0) }
  modify $ \s → s { stInput = newIS }
updateInputState isc = logDebug
  $ "cant process input state change, unknown isc command: " ⧺ show isc
stateKeyPress ∷ KeyFunc → InputState → InputState
stateKeyPress KFScrollUp    is = is { keySt = newks }
  where newks = oldks { keyUp    = True }
        oldks = keySt is
stateKeyPress KFScrollDown  is = is { keySt = newks }
  where newks = oldks { keyDown  = True }
        oldks = keySt is
stateKeyPress KFScrollLeft  is = is { keySt = newks }
  where newks = oldks { keyLeft  = True }
        oldks = keySt is
stateKeyPress KFScrollRight is = is { keySt = newks }
  where newks = oldks { keyRight = True }
        oldks = keySt is
stateKeyPress _          is = is
stateKeyRelease ∷ KeyFunc → InputState → InputState
stateKeyRelease KFScrollUp    is = is { keySt = newks }
  where newks = oldks { keyUp    = False }
        oldks = keySt is
stateKeyRelease KFScrollDown  is = is { keySt = newks }
  where newks = oldks { keyDown  = False }
        oldks = keySt is
stateKeyRelease KFScrollLeft  is = is { keySt = newks }
  where newks = oldks { keyLeft  = False }
        oldks = keySt is
stateKeyRelease KFScrollRight is = is { keySt = newks }
  where newks = oldks { keyRight = False }
        oldks = keySt is
stateKeyRelease _          is = is
stateAccelerate ∷ (Double,Double) → InputState → InputState
stateAccelerate (x,y) is = is { keySt = keySt' }
  where keySt' = (keySt is) { keyAccel = (x,y) }

-- | returns the first key function with this key assigned
lookupKey ∷ KeyMap → Key → KeyFunc
lookupKey (KeyMap keymap) key = if Map.size list > 0
  then fst $ Map.elemAt 0 list
  else KFUnknown $ show key
  where list = Map.filterWithKey (lookupInKeyMap key) keymap

-- original function before hlint)
--lookupKey (KeyMap keymap) key = fst $ Map.elemAt 0
--  $ Map.filterWithKey (\kf ks → (lookupInKeyMap key) kf ks) keymap
lookupInKeyMap ∷ Key → KeyFunc → [Key] → Bool
lookupInKeyMap _  _   []     = False
lookupInKeyMap k0 kf0 (k:ks)
  | k ≡ k0    = True
  | otherwise = lookupInKeyMap k0 kf0 ks

indexKeyMap ∷ KeyMap → KeyFunc → [Key]
indexKeyMap (KeyMap km) kf = fromMaybe [] (Map.lookup kf km)

-- | changes a key mapping in the key map
changeKeyMap ∷ KeyFunc → Key → Int → KeyMap → KeyMap
changeKeyMap kf0 k0 n (KeyMap km)
  = KeyMap $ Map.updateWithKey (changeInKeyMap k0 n) kf0 km
changeInKeyMap ∷ Key → Int → KeyFunc → [Key] → Maybe [Key]
changeInKeyMap k0 1 _  k = Just [k0,last k]
changeInKeyMap k0 2 _  k = Just [head k,k0]
changeInKeyMap _  _ _  k = Just k
--changeKeyMap ∷ KeyFunc → Key → Int → KeyMap → KeyMap
--changeKeyMap kf0 k0 n (KeyMap km)
--  = KeyMap $ Map.mapWithKey (changeInKeyMap kf0 k0 n) km
--changeInKeyMap ∷ KeyFunc → Key → Int → KeyFunc → [Key] → [Key]
--changeInKeyMap kf0 k0 n kf keys
--  | kf0 ≡ kf  = if      (n ≡ 1) then [k0,last keys]
--                else if (n ≡ 2) then [head keys,k0]
--                else                 keys
--  | otherwise = keys

-- | takes key input from glfw and translates it to our format
findKey ∷ GLFW.Key → Key
findKey GLFW.Key'A            = KeyA
findKey GLFW.Key'B            = KeyB
findKey GLFW.Key'C            = KeyC
findKey GLFW.Key'D            = KeyD
findKey GLFW.Key'E            = KeyE
findKey GLFW.Key'F            = KeyF
findKey GLFW.Key'G            = KeyG
findKey GLFW.Key'H            = KeyH
findKey GLFW.Key'I            = KeyI
findKey GLFW.Key'J            = KeyJ
findKey GLFW.Key'K            = KeyK
findKey GLFW.Key'L            = KeyL
findKey GLFW.Key'M            = KeyM
findKey GLFW.Key'N            = KeyN
findKey GLFW.Key'O            = KeyO
findKey GLFW.Key'P            = KeyP
findKey GLFW.Key'Q            = KeyQ
findKey GLFW.Key'R            = KeyR
findKey GLFW.Key'S            = KeyS
findKey GLFW.Key'T            = KeyT
findKey GLFW.Key'U            = KeyU
findKey GLFW.Key'V            = KeyV
findKey GLFW.Key'W            = KeyW
findKey GLFW.Key'X            = KeyX
findKey GLFW.Key'Y            = KeyY
findKey GLFW.Key'Z            = KeyZ
findKey GLFW.Key'0            = Key0
findKey GLFW.Key'1            = Key1
findKey GLFW.Key'2            = Key2
findKey GLFW.Key'3            = Key3
findKey GLFW.Key'4            = Key4
findKey GLFW.Key'5            = Key5
findKey GLFW.Key'6            = Key6
findKey GLFW.Key'7            = Key7
findKey GLFW.Key'8            = Key8
findKey GLFW.Key'9            = Key9
findKey GLFW.Key'Space        = KeySpace
findKey GLFW.Key'Apostrophe   = KeyQuote
findKey GLFW.Key'Comma        = KeyComma
findKey GLFW.Key'Minus        = KeyMinus
findKey GLFW.Key'Period       = KeyPeriod
findKey GLFW.Key'Slash        = KeySlash
findKey GLFW.Key'Semicolon    = KeyColon
findKey GLFW.Key'Equal        = KeyEqual
findKey GLFW.Key'LeftBracket  = KeyBrackLeft
findKey GLFW.Key'RightBracket = KeyBrackRight
findKey GLFW.Key'Backslash    = KeyPipe
findKey GLFW.Key'GraveAccent  = KeyTilde
findKey GLFW.Key'Escape       = KeyEscape
findKey GLFW.Key'Enter        = KeyReturn
findKey GLFW.Key'Tab          = KeyTab
findKey GLFW.Key'Backspace    = KeyBackspace
findKey GLFW.Key'Insert       = KeyInsert
findKey GLFW.Key'Delete       = KeyDelete
findKey GLFW.Key'Up           = KeyUp
findKey GLFW.Key'Down         = KeyDown
findKey GLFW.Key'Right        = KeyRight
findKey GLFW.Key'Left         = KeyLeft
findKey GLFW.Key'PageUp       = KeyPageUp
findKey GLFW.Key'PageDown     = KeyPageDown
findKey GLFW.Key'Home         = KeyHome
findKey GLFW.Key'End          = KeyEnd
findKey GLFW.Key'CapsLock     = KeyCaps
findKey GLFW.Key'ScrollLock   = KeyScrLk
findKey GLFW.Key'NumLock      = KeyNumLk
findKey GLFW.Key'PrintScreen  = KeyPrtSc
findKey GLFW.Key'Pause        = KeyPause
findKey GLFW.Key'F1           = KeyF1
findKey GLFW.Key'F2           = KeyF2
findKey GLFW.Key'F3           = KeyF3
findKey GLFW.Key'F4           = KeyF4
findKey GLFW.Key'F5           = KeyF5
findKey GLFW.Key'F6           = KeyF6
findKey GLFW.Key'F7           = KeyF7
findKey GLFW.Key'F8           = KeyF8
findKey GLFW.Key'F9           = KeyF9
findKey GLFW.Key'F10          = KeyF10
findKey GLFW.Key'F11          = KeyF11
findKey GLFW.Key'F12          = KeyF12
findKey GLFW.Key'F13          = KeyF13
findKey GLFW.Key'F14          = KeyF14
findKey GLFW.Key'F15          = KeyF15
findKey GLFW.Key'F16          = KeyF16
findKey GLFW.Key'F17          = KeyF17
findKey GLFW.Key'F18          = KeyF18
findKey GLFW.Key'F19          = KeyF19
findKey GLFW.Key'F20          = KeyF20
findKey GLFW.Key'F21          = KeyF21
findKey GLFW.Key'F22          = KeyF22
findKey GLFW.Key'F23          = KeyF23
findKey GLFW.Key'F24          = KeyF24
findKey GLFW.Key'F25          = KeyF25
findKey GLFW.Key'Pad0         = KeyNum0
findKey GLFW.Key'Pad1         = KeyNum1
findKey GLFW.Key'Pad2         = KeyNum2
findKey GLFW.Key'Pad3         = KeyNum3
findKey GLFW.Key'Pad4         = KeyNum4
findKey GLFW.Key'Pad5         = KeyNum5
findKey GLFW.Key'Pad6         = KeyNum6
findKey GLFW.Key'Pad7         = KeyNum7
findKey GLFW.Key'Pad8         = KeyNum8
findKey GLFW.Key'Pad9         = KeyNum9
findKey GLFW.Key'PadDecimal   = KeyNumDot
findKey GLFW.Key'PadDivide    = KeyNumDiv
findKey GLFW.Key'PadMultiply  = KeyNumMul
findKey GLFW.Key'PadSubtract  = KeyNumSub
findKey GLFW.Key'PadAdd       = KeyNumAdd
findKey GLFW.Key'PadEnter     = KeyNumRet
findKey GLFW.Key'PadEqual     = KeyNumEql
findKey GLFW.Key'LeftShift    = KeyLShift
findKey GLFW.Key'LeftControl  = KeyLCtrl
findKey GLFW.Key'LeftAlt      = KeyLAlt
findKey GLFW.Key'LeftSuper    = KeyLSuper
findKey GLFW.Key'RightShift   = KeyRShift
findKey GLFW.Key'RightControl = KeyRCtrl
findKey GLFW.Key'RightAlt     = KeyRAlt
findKey GLFW.Key'RightSuper   = KeyRSuper
findKey GLFW.Key'Menu         = KeyMenu
findKey GLFW.Key'World1       = KeyNULL
findKey GLFW.Key'World2       = KeyNULL
findKey GLFW.Key'Unknown      = KeyNULL
--findKey _                     = KeyNULL

-- | printing of key values, leveraging show, this might
--   be faster if it were to be changed to static values
printKeys ∷ [Key] → String
printKeys []     = []
printKeys [k]    = printKey k
printKeys (k:ks) = printKey k ⧺ ", " ⧺ printKeys ks
printKey ∷ Key → String
printKey key = tail $ tail $ tail $ show key
printKeyFunc ∷ KeyFunc → String
printKeyFunc keyFunc = tail $ tail $ show keyFunc
printKeyInd ∷ Int → String
printKeyInd 1 = "1st"
printKeyInd 2 = "2nd"
printKeyInd _ = "NULL"
