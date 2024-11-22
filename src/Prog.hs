{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
-- | the monadic class used to preform low levels
--   of work, provides state, env, io, and error
module Prog
    ( Prog(..), Prog'
    , MonadIO(..), MonadError(..)
    , MonadReader(..), MonadState(..)
    ) where
-- the application is defined
-- as a continuation monad
import UPrelude
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Error.Class ( MonadError(..) )
import Control.Monad.Reader.Class ( MonadReader(..) )
import Control.Monad.State.Class ( MonadState(..), gets )
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Logger.CallStack as Logger
import Data.Tuple (swap)
import Sign.Except ( ProgExcept )
import Sign.Var
    ( atomically, modifyTVar, readTVarIO, writeTVar, TVar )
import Prog.Data ( Env, State(..) )
-- | monadic typeclass instances
--   inlined for exporting
--   ε = env, σ = state
--   α = action, ς = result
newtype Prog ε σ α = Prog {unProg ∷ TVar Env → TVar State
  → (Either ProgExcept α → IO σ) → IO σ}
-- | common case where σ is either
--   an action or and exception
type Prog' ε α = Prog ε (Either ProgExcept α) α
instance Functor (Prog ε σ) where
  fmap f p = Prog $ \e s c → unProg p e s (c ∘ fmap f)
  {-# INLINE fmap #-}
instance Applicative (Prog ε σ) where
  pure x = Prog $ \_ _ → ($ Right x)
  {-# INLINE pure #-}
  pf <*> px = Prog $ \e s c → unProg pf e s
    $ \g → unProg px e s (c ∘ (g <*>))
  {-# INLINE (<*>) #-}
instance Monad (Prog ε σ) where
  return = pure
  {-# INLINE return #-}
  px >>= k = Prog $ \e s c → unProg px e s $ \case
    Right x → unProg (k x) e s c
    Left ex → c (Left ex)
  {-# INLINE (>>=) #-}
-- | provide ability to interface with IO
instance MonadIO (Prog ε σ) where
  liftIO m = Prog $ \_ _ → (Right ⊚ m ⌦)
  {-# INLINE liftIO #-}
-- | throwing and catching errors is for low level errors
--   high level errors are all handled as returning an ADT
instance MonadError ProgExcept (Prog ε σ) where
  throwError e = Prog $ \_ _ → ($ Left e)
  {-# INLINE throwError #-}
  catchError px catcher = Prog $ \e s c → unProg px e s $ \case
    Left ex → unProg (catcher ex) e s c
    Right r → c (Right r)
  {-# INLINE catchError #-}
-- | read-only env
instance MonadReader Env (Prog ε σ) where
  ask = Prog $ \e _ → (Right ⊚ readTVarIO e ⌦)
  {-# INLINE ask #-}
  local _ a = a -- this is a placeholder
  {-# INLINE local #-}
-- | read-write state
instance MonadState State (Prog ε σ) where
  get = Prog $ \_ st → (Right ⊚ readTVarIO st ⌦)
  {-# INLINE get #-}
  put s = Prog $ \_ st → (Right ⊚ atomically (writeTVar st s) ⌦)
  {-# INLINE put #-}
  state f = Prog $ \_ st
    → (Right ⊚ atomically (modifyTVar st (swap ∘ f)) ⌦)
  {-# INLINE state #-}
-- | monadic logger, wasnt working for a while but they updated package
instance Logger.MonadLogger (Prog ε σ) where
  monadLoggerLog loc ls ll msg = do
    lf ← gets stLogFunc
    liftIO $ lf loc ls ll (Logger.toLogStr msg)
  {-# INLINE monadLoggerLog #-}
