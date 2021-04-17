module Main where

import Interpreter
import CState
import FunParser
import Parsing

main :: IO ()
main = dialog funParser obey (init_env, init_gs)
