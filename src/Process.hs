module Process where


import           Control.Applicative
import           Control.Arrow       ((&&&))
import           Control.Lens
import           Control.Monad       ((<=<))
import           Data.Char           (isAlphaNum, toLower)
import           Data.Foldable
import           Data.List.Split
import qualified Data.Map.Strict     as M
import           Data.Maybe
import           Data.Thyme
import           Text.Pandoc.JSON

import           Types
import           Utils


process :: Pandoc -> Pandoc
process (Pandoc meta blocks) =
    Pandoc meta
               . flattenYearIndex
               . foldl' indexGroup M.empty
               . mapMaybe (   sequenceA
                          .   (id &&& parseZonedHeader . fold . header)
                          <=< blocksToTree
                          )
           $ split' blocks

split' :: [Block] -> [[Block]]
split' = filter (not . null) . split (keepDelimsL $ whenElt isH1)

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

mkHeader :: Int -> String -> Block
mkHeader level text =
    Header level
               (replaceOn (not . isAlphaNum) '-' $ map toLower text, [], [])
               [Str text]

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
