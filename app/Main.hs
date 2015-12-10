{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Control.Applicative
import           Control.Arrow        ((&&&))
import           Control.Lens
import           Control.Monad        ((<=<))
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Char            (isAlphaNum, toLower)
import           Data.Foldable
import           Data.List.Split
import qualified Data.Map.Strict      as M
import           Data.Maybe
import           Data.Thyme
import           System.Locale
import           Text.Pandoc.JSON


main :: IO ()
main = B.interact (either error encode . fmap process . eitherDecode)


data HeaderGroup = HG Block [Block]
data Entry       = Entry LocalTime HeaderGroup
type YearIndex   = M.Map Year MonthIndex
type MonthIndex  = M.Map Month DayIndex
type DayIndex    = M.Map DayOfMonth TimeIndex
type TimeIndex   = M.Map TimeOfDay HeaderGroup


process :: Pandoc -> Pandoc
process (Pandoc meta blocks) =
    Pandoc meta
               . flattenYearIndex
               . foldl' indexGroup M.empty
               . mapMaybe (   sequenceA
                          .   (id &&& parseZonedHeader . fold . header)
                          <=< blocksToTree
                          )
           $ split (keepDelimsR $ whenElt isH1) blocks

isH1 :: Block -> Bool
isH1 (Header 1 _ _) = True
isH1 _              = False

blocksToTree :: [Block] -> Maybe HeaderGroup
blocksToTree (h1@(Header 1 _ _) : blocks) = Just $ HG h1 blocks
blocksToTree _                            = Nothing

header :: HeaderGroup -> Maybe String
header (HG (Header _ _ str) _) = Just $ foldMap inlineText str
header _                       = Nothing

indexGroup :: YearIndex -> (HeaderGroup, ZonedTime) -> YearIndex
indexGroup yi (hg, ZonedTime (LocalTime lday time) _) = M.alter mindex year yi
    where
      (YearMonthDay year month day) = lday ^. gregorian

      ti' = M.singleton time  hg
      di' = M.singleton day   ti'
      mi' = M.singleton month di'

      mindex :: Maybe MonthIndex -> Maybe MonthIndex
      mindex (Just mi) = Just $ M.alter dindex month mi
      mindex Nothing   = Just mi'

      dindex :: Maybe DayIndex -> Maybe DayIndex
      dindex (Just di) = Just $ M.alter tindex day di
      dindex Nothing   = Just di'

      tindex :: Maybe TimeIndex -> Maybe TimeIndex
      tindex (Just ti) = Just $ M.insert time hg ti
      tindex Nothing   = Just ti'

flattenYearIndex :: YearIndex -> [Block]
flattenYearIndex = concatMap (uncurry flattenMonthIndex) . M.toList

flattenMonthIndex :: Year -> MonthIndex -> [Block]
flattenMonthIndex year = (mkHeader 1 (show year) :)
                         . concatMap (uncurry (flattenDayIndex year))
                         . M.toList

flattenDayIndex :: Year -> Month -> DayIndex -> [Block]
flattenDayIndex year month =
    (mkHeader 2 hdr :) . concatMap (uncurry ( flattenTimeIndex
                                            . YearMonthDay year month))
                           . M.toList
    where
      hdr = formatTime' "%Y-%m %B" $ YearMonthDay year month 1

flattenTimeIndex :: YearMonthDay -> TimeIndex -> [Block]
flattenTimeIndex ymd =
    (mkHeader 3 (foldMap (formatTime' "%F %A") lt) :)
    . concatMap (uncurry ( flattenHeaderGroup
                         . liftA2 LocalTime (gregorianValid ymd)
                         . Just
                         ))
          . M.toList
    where
      lt = LocalTime <$> gregorianValid ymd <*> pure midnight

flattenHeaderGroup :: Maybe LocalTime -> HeaderGroup -> [Block]
flattenHeaderGroup Nothing   _         = []
flattenHeaderGroup (Just lt) (HG _ bs) =
    mkHeader 4 (formatTime' "<%Y-%m-%d %a %H:%M>" lt) : bs

formatTime' :: FormatTime t => String -> t -> String
formatTime' = formatTime defaultTimeLocale

parseDateHeader :: (ParseTime t, Show t) => String -> Maybe t
parseDateHeader input =
        parseTime defaultTimeLocale "Date: %a, %d %b %Y %T %z" input
    <|> parseTime defaultTimeLocale "Date: %a, %d %b %Y" input

parseZonedHeader :: String -> Maybe ZonedTime
parseZonedHeader = parseDateHeader

mkHeader :: Int -> String -> Block
mkHeader level text =
    Header level
               (replaceOn (not . isAlphaNum) '-' $ map toLower text, [], [])
               [Str text]

replaceOn :: (Char -> Bool) -> Char -> String -> String
replaceOn f r (c:cs)
    | f c        = r : replaceOn f r cs
    | otherwise  = c : replaceOn f r cs
replaceOn _ _ [] = []

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
