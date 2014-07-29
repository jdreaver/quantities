import System.Environment
import System.Exit

import Data.Quantities

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse []     = usage   >> exit
parse [s]    = putStr $ (++ "\n") $ either show show $ fromString s
parse _      = usage   >> die

usage   = putStrLn "Usage: quantities [-vh] expression"
version = putStrLn "Haskell quantities 0.3.0"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
