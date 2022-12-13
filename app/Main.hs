module Main (main) where

import Durnir.Exec (exec)
import Durnir.Options (parseOptions)

main :: IO ()
main = exec =<< parseOptions
