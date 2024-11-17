-- | these are all either ADTs or simple structures
--   made up of ints, strings, floats, etc...
module Data where
-- the most generic structures are found
import Prelude()
import Data.Map (Map)
import UPrelude

-- | stack is a simple stack with pop/stash
type Stack α = [α]

-- | generic 0-255 rgba structure
data Color = Color Int Int Int Int deriving (Show, Eq)

-- | cardinal directions
data Cardinal = North | South | West | East
              | NorthWest | NorthEast | SouthWest
              | SouthEast | CardNULL deriving (Show, Eq)

-- | cards represents cardinal tiles in a 2-3d grid, with
--   Nothing representing the edge of the grid
data Cards α = Cards   { cNorth ∷ Maybe α
                       , cSouth ∷ Maybe α
                       , cEast  ∷ Maybe α
                       , cWest  ∷ Maybe α }
             | Cards8W { cNorth ∷ Maybe α
                       , cSouth ∷ Maybe α
                       , cEast  ∷ Maybe α
                       , cWest  ∷ Maybe α
                       , cNW    ∷ Maybe α
                       , cNE    ∷ Maybe α
                       , cSW    ∷ Maybe α
                       , cSE    ∷ Maybe α }
             | Cards3D { cNorth ∷ Maybe α
                       , cSouth ∷ Maybe α
                       , cEast  ∷ Maybe α
                       , cWest  ∷ Maybe α
                       , cAbove ∷ Maybe α
                       , cBelow ∷ Maybe α } deriving (Show, Eq)

-- | print args logInfo specific info from the lowest levels
data PrintArg = PrintNULL deriving (Show, Eq)

-- | fps defined as actual and desired,
--   and whether or not to display
data FPS = FPS Double Int Bool deriving (Show, Eq)

-- | generic camera for movement
data Camera = Camera { cam ∷ (Double,Double,Double)
                     , mov ∷ (Double,Double) } deriving (Show, Eq)

-- | possible difficulties
data Difficulty = DNormal
                | DHard
                | DHarder
                | DInsane
                | DDisabled
                | DEasy
                | DNULL deriving (Show, Eq)

-- | towns has a few base maps
data MapType = MapNormal
             | MapDesert
             | MapJungle
             | MapMixed
             | MapSnow
             | MapMountains
             | MapNULL deriving (Show, Eq)

-- | the map is created with certain paraeters
data MapSettings = MapSettings
       { msBuried ∷ BuriedStatus
       , msType   ∷ MapType
       , msSize   ∷ (Int,Int) } deriving (Show, Eq)

-- | one of the options to map settings
data BuriedStatus = NoBuried | LocalBuried
                  | LoadBuried String deriving (Show, Eq)

-- | map data is held as an array with corresponding size
data MapTiles = MapTiles (Int,Int) (Space MapTile) deriving (Show, Eq)

-- | a map tile contains the tile number and index state
data MapTile = MapTile Int Int deriving (Eq)
instance Show MapTile where
  show (MapTile n s) = show n

-- | collections of map tiles are classified in data
type Space α = [Plane α]
type Plane α = [Row α]
type Row α = [α]

-- TODO: move this to a better place
-- | lua shell executes commands in global state
data Shell = Shell { shPrompt ∷ String
                   , shTabbed ∷ Maybe Int
                   , shCursor ∷ Int
                   , shInpStr ∷ String
                   , shCache  ∷ String
                   , shOutStr ∷ String
                   , shRet    ∷ String
                   , shLoaded ∷ Bool
                   , shHistI  ∷ Int
                   , shHist   ∷ [String] } deriving (Show, Eq)

-- abstract popup, independent of window
data Popup = Popup { puPos  ∷ (Double,Double)
                   , puSize ∷ (Double,Double)
                   , puType ∷ PopupType } deriving (Show, Eq)

-- different pop ups have different functionality
data PopupType = PopupSetKey Int KeyFunc [Key]
               | PopupSavename String
               | PopupNULL deriving (Show, Eq)


-- | possible keys, so we dont have to use the GLFW one everywhere
data Key = KeyA
         | KeyB
         | KeyC
         | KeyD
         | KeyE
         | KeyF
         | KeyG
         | KeyH
         | KeyI
         | KeyJ
         | KeyK
         | KeyL
         | KeyM
         | KeyN
         | KeyO
         | KeyP
         | KeyQ
         | KeyR
         | KeyS
         | KeyT
         | KeyU
         | KeyV
         | KeyW
         | KeyX
         | KeyY
         | KeyZ
         | KeyUp
         | KeyLeft
         | KeyDown
         | KeyRight
         | KeyTilde
         | Key1
         | Key2
         | Key3
         | Key4
         | Key5
         | Key6
         | Key7
         | Key8
         | Key9
         | Key0
         | KeyMinus
         | KeyEqual
         | KeyBackspace
         | KeyTab
         | KeyBrackLeft
         | KeyBrackRight
         | KeyPipe
         | KeyCaps
         | KeyColon
         | KeyQuote
         | KeyReturn
         | KeyComma
         | KeyPeriod
         | KeySlash
         | KeySpace
         | KeyEscape
         | KeyNum0
         | KeyNum1
         | KeyNum2
         | KeyNum3
         | KeyNum4
         | KeyNum5
         | KeyNum6
         | KeyNum7
         | KeyNum8
         | KeyNum9
         | KeyNumDot
         | KeyNumDiv
         | KeyNumMul
         | KeyNumSub
         | KeyNumAdd
         | KeyNumRet
         | KeyNumEql
         | KeyF1
         | KeyF2
         | KeyF3
         | KeyF4
         | KeyF5
         | KeyF6
         | KeyF7
         | KeyF8
         | KeyF9
         | KeyF10
         | KeyF11
         | KeyF12
         | KeyF13
         | KeyF14
         | KeyF15
         | KeyF16
         | KeyF17
         | KeyF18
         | KeyF19
         | KeyF20
         | KeyF21
         | KeyF22
         | KeyF23
         | KeyF24
         | KeyF25
         | KeyLShift
         | KeyLCtrl
         | KeyLAlt
         | KeyLSuper
         | KeyRShift
         | KeyRCtrl
         | KeyRAlt
         | KeyRSuper
         | KeyInsert
         | KeyDelete
         | KeyPageUp
         | KeyPageDown
         | KeyHome
         | KeyEnd
         | KeyScrLk
         | KeyNumLk
         | KeyPrtSc
         | KeyPause
         | KeyMenu
         | KeyUnknown String
         | KeyNULL deriving (Show, Eq, Ord)

-- | a mapping from key functionality to none or more
--   physical keys.  used in the child threads so can be lazy.
--   hlint suggests newtype here
newtype KeyMap = KeyMap (Map KeyFunc [Key]) deriving (Show,Eq,Ord)

-- | ADTs for the key layouts each KeyFunc
--   corresponds to none or many physical Keys
--   some of these are engine related, some
--   are from the original game.
data KeyFunc = KFUp
             | KFDown
             | KFLeft
             | KFRight
             | KFLvlUp
             | KFLvlDown
             | KFScrollUp
             | KFScrollDown
             | KFScrollLeft
             | KFScrollRight
             | KFEscape
             | KFReturn
             | KFShell
             | KFFullScreen
             | KFTest
             | KFTest2
             | KFUnknown String
             | KFNULL deriving (Show, Eq, Ord)
-- | various states of loading for the UI
data LoadState = Loaded | Unloaded | Loading String deriving (Show, Eq)
