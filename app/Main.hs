{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Control.Applicative
import           Control.Arrow
import qualified Data.Map            as M
import           Data.Thyme
import           Data.Thyme.Format
import           Debug.Trace
import           System.Locale
import           Text.Pandoc.JSON


main :: IO ()
main = toJSONFilter expandHeaders


expandHeaders :: Block -> [Block]
expandHeaders h@(Header 1 _attrs header) =
    traceShow (id &&& parseZonedHeader $ foldMap inlineText header) [h]
expandHeaders block = [block]

parseDateHeader :: (ParseTime t, Show t) => String -> Maybe t
parseDateHeader input =
        parseTime defaultTimeLocale "Date: %a, %d %b %Y %T %z" input
    <|> parseTime defaultTimeLocale "Date: %a, %d %b %Y" input

parseZonedHeader :: String -> Maybe ZonedTime
parseZonedHeader = parseDateHeader

inlineText :: Inline -> String
inlineText (Str str)         = str
inlineText (Emph ils)        = foldMap inlineText ils
inlineText (Strong ils)      = foldMap inlineText ils
inlineText (Strikeout ils)   = foldMap inlineText ils
inlineText (Superscript ils) = foldMap inlineText ils
inlineText (Subscript ils)   = foldMap inlineText ils
inlineText (SmallCaps ils)   = foldMap inlineText ils
inlineText (Quoted _ ils)    = foldMap inlineText ils
inlineText (Cite _ ils)      = foldMap inlineText ils
inlineText (Code _ str)      = str
inlineText Space             = " "
inlineText LineBreak         = "\n"
inlineText (Math _ str)      = str
inlineText (RawInline _ str) = str
inlineText (Link ils _)      = foldMap inlineText ils
inlineText (Image ils _)     = foldMap inlineText ils
inlineText (Note _)          = ""
inlineText (Span _ ils)      = foldMap inlineText ils
