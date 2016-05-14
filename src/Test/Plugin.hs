{-# LANGUAGE FlexibleContexts #-}

module Test.Plugin
    (inspectBuffer
    ,fuzzyDate
    ) where

import Neovim

import Control.Monad
import Data.Dates
import Data.Either
import Data.Fixed
import Data.Maybe
import Data.Time
import qualified Text.Parsec as P
import Text.Printf

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

-- ######################################################################

try :: (Show a) => Either a b -> Neovim r st b
try = either (err . text . show) return

objToInt :: Object -> Neovim r st Int
objToInt o =
  case o of
    ObjectUInt n -> return $ toInt n
    ObjectInt n  -> return $ toInt n
    _            -> err $ text $ show o
  where toInt   :: (Integral a) => a -> Int
        toInt    = fromInteger . toInteger

-- | Helper function that calls the @input()@ function of neovim.
input :: String       -- ^ Message to display
      -> Maybe String -- ^ Input fiiled in
      -> Maybe String -- ^ Completion mode
      -> Neovim r st (Either Object Object)
input message mPrefilled mCompletion =
  do r1_o <- vim_call_function "inputsave" [] >>= try
     r1   <- objToInt r1_o
     when (r1 /= 0) (err $ text "inputsave return OOM")

     let args = catMaybes (map (toObject <$>) [Just message, mPrefilled, mCompletion])
     res <- vim_call_function "input" args

     r2_o <- vim_call_function "inputrestore" [] >>= try
     r2   <- objToInt r2_o
     when (r2 /= 0) (err $ text "nothing to restore")

     return res

askForDate :: Neovim r st DateTime
askForDate =
  do currDate <- liftIO $ getCurrentDateTime
     obj      <- input "Enter date: "    -- msg
                       (Just "today")    -- input filled in
                       Nothing >>= try   -- completion mode
     relDate <- try $ fromObject obj

     try $ parseDate currDate relDate

askForFormat :: Neovim r st String
askForFormat =
  do obj <- input "Enter format: "         -- msg
                  (Just "<%Y-%m-%d %a>") -- input filled in
                  Nothing >>= try        -- completion mode
     try $ fromObject obj

defInterval :: DateInterval
defInterval = Days 0

parseInputInterval :: String -> Neovim r st DateInterval
parseInputInterval s =
  do (c,i) <- try $ P.runParser myIntervalParser () "" s
     return $ if c == '-'
                 then negateInterval i
                 else i
    where myIntervalParser :: P.Stream s m Char => P.ParsecT s st m (Char,DateInterval)
          myIntervalParser = (,) <$> (P.option '+' (P.oneOf "+-"))
                                 <*> (P.spaces *> pDateInterval)


askForInterval :: Neovim r st DateInterval
askForInterval =
  do obj <- input "Interval [(+,-) n (day,week,month,year)[s]: " -- msg
                  Nothing                                         -- input filled in
                  Nothing >>= try                                 -- completion mode
     s  <- try $ fromObject obj
     if null s
        then return defInterval
        else parseInputInterval s

fuzzyDate :: Neovim r st ()
fuzzyDate =
  do d'       <- askForDate
     format   <- askForFormat
     interval <- askForInterval
     let d        = addInterval d' interval
         gregDay  = fromGregorian (toInteger $ year d) (month d) (day d)
         psec     = MkFixed ((toInteger $ second d) * (10^12))
         Just tod = makeTimeOfDayValid (hour d) (minute d) psec
         locTime  = LocalTime {localDay = gregDay, localTimeOfDay = tod}
         str      = formatTime defaultTimeLocale format locTime
     vim_command (printf "normal i%s" str) >>= try
     return ()

