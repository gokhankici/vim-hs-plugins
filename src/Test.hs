{-# LANGUAGE TemplateHaskell #-}
module Test (plugin) where

import Neovim
import Test.Plugin

plugin :: Neovim (StartupConfig NeovimConfig) () NeovimPlugin
plugin = wrapPlugin Plugin
           { exports         = []
           , statefulExports = [ ((), (), [ $(function' 'inspectBuffer) Sync ])
                               , ((), (), [ $(function' 'fuzzyDate)     Sync ]) ]
           }
