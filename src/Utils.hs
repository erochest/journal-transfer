module Utils where


import           Control.Applicative
import           Data.Thyme
import           System.Locale


formatTime' :: FormatTime t => String -> t -> String
formatTime' = formatTime defaultTimeLocale

parseDateHeader :: (ParseTime t, Show t) => String -> Maybe t
parseDateHeader input =
        parseTime defaultTimeLocale "Date: %a, %d %b %Y %T %z" input
    <|> parseTime defaultTimeLocale "Date: %a, %d %b %Y" input

parseZonedHeader :: String -> Maybe ZonedTime
parseZonedHeader = parseDateHeader

replaceOn :: (Char -> Bool) -> Char -> String -> String
replaceOn f r (c:cs)
    | f c        = r : replaceOn f r cs
    | otherwise  = c : replaceOn f r cs
replaceOn _ _ [] = []
