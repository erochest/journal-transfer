module Types where


import qualified Data.Map.Strict  as M
import           Data.Thyme
import           Text.Pandoc.JSON


data HeaderGroup = HG Block [Block]
data Entry       = Entry LocalTime HeaderGroup
type YearIndex   = M.Map Year MonthIndex
type MonthIndex  = M.Map Month DayIndex
type DayIndex    = M.Map DayOfMonth TimeIndex
type TimeIndex   = M.Map TimeOfDay HeaderGroup
