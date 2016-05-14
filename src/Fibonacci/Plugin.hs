module Fibonacci.Plugin (fibonacci) where

import Neovim

-- | Neovim is not really good with big numbers, so we return a String here.
fibonacci :: Object -> Neovim' String
fibonacci o =
  case o of
    ObjectUInt n -> helper n
    ObjectInt n  -> helper n
    _            -> err $ text $ show o
  where helper n = return . show $ fibs !! (toInt n)
        toInt   :: (Integral a) => a -> Int
        toInt    = fromInteger . toInteger
        fibs     = 0:1:(scanl1 (+) fibs)
