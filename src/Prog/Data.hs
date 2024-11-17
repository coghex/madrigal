-- | data for the continuation monad
{-# LANGUAGE RankNTypes #-}
module Prog.Data where
-- the state is
import Prelude()
import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import Data ( FPS(..), Difficulty(..)
            , Key, KeyFunc, KeyMap )
import Data.Time.Clock.System ( SystemTime )
import Sign.Data ( Event, TState )
import Sign.Except ( ProgExcept )
import Sign.Queue ( Queue, TChan )
import Sign.Var ( TVar )

-- | specific utility actions
data ProgResult = ProgSuccess | ProgError deriving (Show, Eq)
-- | glfw loop status
data LoopControl = ContinueLoop | AbortLoop deriving Eq

-- | env should only hold pointers/references
data Env = Env { envEventQ ∷ Queue Event
	       }

-- | state holds mutable data, and the
--   current status of the whole App
data State = State { stStatus   ∷ ProgExcept
                   -- logging monadic function
                   , stLogFunc  ∷ Logger.Loc → Logger.LogSource
                                    → Logger.LogLevel → Logger.LogStr
                                    → IO ()
                   , stStartT    ∷ !SystemTime
                   }
