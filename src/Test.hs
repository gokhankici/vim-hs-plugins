{-# LANGUAGE TemplateHaskell #-}
module Test (plugin) where

import Neovim
import Test.Plugin (inspectBuffer)

plugin :: Neovim (StartupConfig NeovimConfig) () NeovimPlugin
plugin = wrapPlugin Plugin
           { exports         = []
           , statefulExports = [ ((), (), [ $(function' 'inspectBuffer) Sync ]) ]
           }
