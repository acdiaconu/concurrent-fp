module Main where

import Interpreter

main :: IO ()
main = dialog funParser obey (empty_env, init_mem)
