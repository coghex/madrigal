-- | this is an interface module, that exports the original GLFW with
--   some extra structures and functions attached
module Vulk.GLFW
  ( module Graphics.UI.GLFW
  , KeyLayout (..)
  , keyCheck
  , numCheck
  , calcInpKey
  , mousebutt1
  , mousebutt2
  , mousebutt3
  ) where
-- the input is defined for glfw, since
-- the library is just one giant unified
-- C library, we dont want to import
-- everything all at once, and this way
-- we can abstract to other window managers
import Prelude()
import UPrelude
import Data.Char (toUpper,toLower)
import Graphics.UI.GLFW

-- | TODO: remove all instances of this
data KeyLayout = KeyLayout
  { klEsc ∷ String
  , klRet ∷ String
  , klDel ∷ String
  , klSpc ∷ String
  , klTab ∷ String
  , klUp  ∷ String
  , klLft ∷ String
  , klDwn ∷ String
  , klRgt ∷ String
  , klSh  ∷ String } deriving (Show, Eq)

-- | mousebutton synonyms
mousebutt1 ∷ MouseButton
mousebutt1 = MouseButton'1
mousebutt2 ∷ MouseButton
mousebutt2 = MouseButton'2
mousebutt3 ∷ MouseButton
mousebutt3 = MouseButton'3
--mousebutt4 ∷ GLFW.MouseButton
--mousebutt4 = GLFW.MouseButton'4
--mousebutt5 ∷ GLFW.MouseButton
--mousebutt5 = GLFW.MouseButton'5
--mousebutt6 ∷ GLFW.MouseButton
--mousebutt6 = GLFW.MouseButton'6
--mousebutt7 ∷ GLFW.MouseButton
--mousebutt7 = GLFW.MouseButton'7
--mousebutt8 ∷ GLFW.MouseButton
--mousebutt8 = GLFW.MouseButton'8

-- | translates keys from strings for ease of use
-- | TODO: delete this
getGLFWKey ∷ String → Key
getGLFWKey "ESC" = Key'Escape
getGLFWKey "RET" = Key'Enter
getGLFWKey "DEL" = Key'Backspace
getGLFWKey "SPC" = Key'Space
getGLFWKey "TAB" = Key'Tab
getGLFWKey "LFT" = Key'Left
getGLFWKey "RGT" = Key'Right
getGLFWKey "UPP" = Key'Up
getGLFWKey "DWN" = Key'Down
getGLFWKey "SH"  = Key'GraveAccent
getGLFWKey "C"   = Key'C
getGLFWKey "E"   = Key'E
getGLFWKey "R"   = Key'R
getGLFWKey "H"   = Key'H
getGLFWKey "J"   = Key'J
getGLFWKey "K"   = Key'K
getGLFWKey "L"   = Key'L
getGLFWKey "W"   = Key'W
getGLFWKey "A"   = Key'A
getGLFWKey "S"   = Key'S
getGLFWKey "D"   = Key'D
getGLFWKey _     = Key'Unknown

-- | TODO: delete this
keyCheck ∷ Bool → KeyLayout → Key → String → Bool
keyCheck cap keyLayout k str
  | cap       = False
  | otherwise = (k ≡ (getGLFWKey nk))
  where nk = applyKeyLayout keyLayout str

-- | glfw keys to integers
numCheck ∷ Key → Int
numCheck Key'0 = 0
numCheck Key'1 = 1
numCheck Key'2 = 2
numCheck Key'3 = 3
numCheck Key'4 = 4
numCheck Key'5 = 5
numCheck Key'6 = 6
numCheck Key'7 = 7
numCheck Key'8 = 8
numCheck Key'9 = 9
numCheck _     = -1

-- | TODO: delete this 
applyKeyLayout ∷ KeyLayout → String → String
applyKeyLayout keyLayout "ESC" = klEsc keyLayout
applyKeyLayout keyLayout "RET" = klRet keyLayout
applyKeyLayout keyLayout "DEL" = klDel keyLayout
applyKeyLayout keyLayout "SPC" = klSpc keyLayout
applyKeyLayout keyLayout "TAB" = klTab keyLayout
applyKeyLayout keyLayout "LFT" = klLft keyLayout
applyKeyLayout keyLayout "RGT" = klRgt keyLayout
applyKeyLayout keyLayout "UP"  = klUp  keyLayout
applyKeyLayout keyLayout "DWN" = klDwn keyLayout
applyKeyLayout keyLayout "SH"  = klSh  keyLayout
applyKeyLayout _         "LFA" = "LFT"
applyKeyLayout _         "RTA" = "RGT"
applyKeyLayout _         "UPA" = "UPP"
applyKeyLayout _         "DNA" = "DWN"
applyKeyLayout _         "H"   = "H"
applyKeyLayout _         "J"   = "J"
applyKeyLayout _         "K"   = "K"
applyKeyLayout _         "L"   = "L"
applyKeyLayout _         "C"   = "C"
applyKeyLayout _         "A"   = "A"
applyKeyLayout _         "R"   = "R"
applyKeyLayout _         "E"   = "E"
applyKeyLayout _         _     = "NULL"

-- | convert a known key into possible a string
getKeyStr ∷ Key → IO (Maybe String)
getKeyStr k = getKeyName k 0

-- | takes the input key and applys shift if neccesary
calcInpKey ∷ Key → ModifierKeys → IO String
calcInpKey k mk = do
  inp ← getKeyStr k
  case inp of
    Just str → return $ applyMod mk str
    Nothing  → return ""

-- | takes a string and applys uppercase on glfw shift
applyMod ∷ ModifierKeys → String → String
applyMod mk str = if modifierKeysShift mk
  then map upcase str else map downcase str

-- | different keys have different uppercase results, toUpper from
--   Data.Char can handle all of the alphabet chars
upcase ∷ Char → Char
upcase '1'  = '!'
upcase '2'  = '@'
upcase '3'  = '#'
upcase '4'  = '$'
upcase '5'  = '%'
upcase '6'  = '^'
upcase '7'  = '&'
upcase '8'  = '*'
upcase '9'  = '('
upcase '0'  = ')'
upcase '-'  = '_'
upcase '='  = '+'
upcase '['  = '{'
upcase ']'  = '}'
upcase '\\' = '|'
upcase ';'  = ':'
upcase '\'' = '"'
upcase ','  = '<'
upcase '.'  = '>'
upcase '/'  = '?'
upcase '`'  = '~'
upcase c    = toUpper c
-- | i dont know why one would want this but here it is
--   Data.Char can handle all of the alphabet chars
downcase ∷ Char → Char
downcase '!' = '1'
downcase '@' = '2'
downcase '#' = '3'
downcase '$' = '4'
downcase '%' = '5'
downcase '^' = '6'
downcase '&' = '7'
downcase '*' = '8'
downcase '(' = '9'
downcase ')' = '0'
downcase '_' = '-'
downcase '+' = '='
downcase '{' = '['
downcase '}' = ']'
downcase '|' = '\\'
downcase ':' = ';'
downcase '"' = '\''
downcase '<' = ','
downcase '>' = '.'
downcase '?' = '/'
downcase '~' = '`'
downcase c  = toLower c
