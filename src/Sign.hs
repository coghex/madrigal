{-# LANGUAGE Strict #-}
-- | the Sign submodules dont really have anything to do
--   with this function.  this function really only does
--   one thing and the rest of Sign is more of an interface
--   to the common stm and threading libraries
module Sign where
-- overarching error handling
import System.Exit ( exitFailure )
import Sign.Except ( displayException, ProgExcept )
-- | fails IO, thus everything,
--   when an error is returned
--   by any of the submodules.
checkStatus ∷ Either ProgExcept () → IO ()
checkStatus (Right ()) = pure ()
checkStatus (Left err) = do
  putStrLn $ displayException err
  exitFailure
