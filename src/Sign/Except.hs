{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | exceptions are errors that will kill the whole program
module Sign.Except
  ( ProgExcept(..), ExType(..), Exceptable
  , displayException, testEx ) where
-- exceptions are printed
import Prelude()
import UPrelude
import Control.Exception (Exception, displayException)
import Type.Reflection
-- exceptions contain possibly a
-- code, and two strings. the
-- msg contains a possible error
-- message. exTy is the name of the
-- module throwing the exception

-- | for an exception to be exceptable it must have these attributes
type Exceptable ς = (Typeable ς, Eq ς, Show ς)
-- | exceptions are either coming from the main thread in the form of
--   vulkan, glfw, or prog exception, exceptions in threads are handled
--   at a higher level and shouldnt ever crash the entire program
data ExType = ExNull | ExProg | ExVulk deriving (Show, Eq)
-- | an exception holds a code, a type and a message
data ProgExcept = ∀ ς. (Exceptable ς) ⇒ ProgExcept
       { code ∷ Maybe ς
       -- ^ as long as the library being used has exceptions
       --   with typeable, show, and eq, we can display the
       --   corresponding code and error from the libary
       , exTy ∷ ExType
       , msg  ∷ String }
-- | we have some instances for the exceptions that we have created
instance Show ProgExcept where
  show (ProgExcept (Just code) exTy msg)
    = show code ⧺ " " ⧺ show msg ⧺ " " ⧺ show exTy
  show (ProgExcept Nothing exTy msg) = show msg ⧺ show exTy
  
-- | actions on when an exception in the program has occured,
--   i have yet to see this happen except when i
--   intentionally triggered it
instance Exception ProgExcept where
  displayException (ProgExcept Nothing exTy msg) = unlines
    [ ""
    , show exTy ⧺ " exception:"
    , "*** " ⧺ msg ]
  displayException (ProgExcept (Just c) exTy msg) = unlines
    [ ""
    , show exTy ⧺ " error: " ⧺ show c
    , "*** " ⧺ msg ]

-- | provides equality for exceptions
--   regardless of type
testEx ∷ ∀ ς. Exceptable ς ⇒ ProgExcept → ς → Bool
testEx (ProgExcept (Just r) _ _) ex
  = case eqTypeRep (typeRep @ς) (typeOf r) of
    Just HRefl → r == ex
    Nothing → False
testEx (ProgExcept Nothing _ _) _ = False
