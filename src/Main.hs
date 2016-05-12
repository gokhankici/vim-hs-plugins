module Main where

import Neovim

import qualified Fibonacci as Fibonacci
import qualified Random    as Random
import qualified Test      as Test

main :: IO ()
main = neovim defaultConfig
         { plugins = plugins defaultConfig ++ [ Fibonacci.plugin
                                              , Random.plugin
                                              , Test.plugin      ]
         }
