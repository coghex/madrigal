{-# LANGUAGE Strict #-}
module Main where
-- where the magic happens...
import Prelude()
import UPrelude
import Sign ( checkStatus )
import Prog.Init ( runProg )
import Vulk ( runVulk )

main ∷ IO ()
main = runProg checkStatus runVulk
