-- | functions to help the threads are here, it used
--   to have a ton of stuff, but the threads have since
--   been simplified
module Sign.Thread where
-- some helper threading functions can be found
import qualified Control.Concurrent as CC

-- | idles the CPU so that the threads are
--   not taking up compute time that we need
threadDelay :: Int -> IO ()
threadDelay = CC.threadDelay
