module Test.Plugin (inspectBuffer) where

import Neovim
import Control.Monad

inspectBuffer :: Neovim r st ()
inspectBuffer = do
  cb      <- vim_get_current_buffer
  isValid <- buffer_is_valid cb
  when isValid $ do
    let newName = "magic"
    retval <- buffer_set_name cb newName
    case retval of
      Right _ -> return ()
      Left e  -> (err . text . show) e

