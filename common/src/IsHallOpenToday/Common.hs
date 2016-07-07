module IsHallOpenToday.Common (
    module IsHallOpenToday.Common
  , module X
)   where

import IsHallOpenToday.HallDates as X
import IsHallOpenToday.Css as X

someFunc :: IO ()
someFunc = print =<< getTime
