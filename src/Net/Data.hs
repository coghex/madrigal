module Net.Data where
import Prelude()
import UPrelude
import System.IO ( Handle(..) )

-- | a collection of data refering to the state of network traffic
data NetState       = NetState { netStateServices ∷ [NetService]
                               } deriving (Show, Eq)
data NetService     = NetService { nsType   ∷ NetServiceType
                                 , nsStatus ∷ NetStatus
                                 , nsPort   ∷ Int
                                 } deriving (Show, Eq)
data NetAction      = NetActionNewService NetServiceType
                    | NetActionTest NetTestParam
                    | NetActionOutp Handle String
                    | NetActionNULL deriving (Show, Eq)
data NetServiceType = NSCommand | NSNULL deriving (Show, Eq)
data NetResult      = ResNetSuccess | ResNetError String
                    | ResNetNewService NetService
                    | ResNetNULL deriving (Show, Eq)
data NetTestParam   = NetTestPrintNet
                    | NetTestNULL deriving (Show, Eq)
data NetStatus      = NetStatusStarting
                    | NetStatusRunning
                    | NetStatusEnding
                    | NetStatusNULL deriving (Show, Eq)
