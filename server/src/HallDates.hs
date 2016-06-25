{-# LANGUAGE OverloadedStrings #-}    
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}

module HallDates (
    module HallDates
)   where


import BasicPrelude

import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Functor.Identity

class (Monad m) => MonadTime m where
    getTime :: m UTCTime

instance MonadTime IO where
    getTime = getCurrentTime

instance MonadTime Identity where
    getTime = pure $ UTCTime (ModifiedJulianDay 57564) (secondsToDiffTime 82438)

data Message = NoHallToday
             | BreakFastToday
             | LunchToday
             | BrunchToday
             | DinnerToday
             | MCRDinnerToday
             | NoHallForAges
             | MaybeHall
             deriving (Show, Eq, Ord, Read, Enum, Bounded)

interpret :: Message -> Text
interpret NoHallToday    = "Hall is closed today."
interpret BreakFastToday = "Hall is serving breakfast today."
interpret LunchToday     = "Hall is serving lunch today."
interpret BrunchToday    = "Hall is serving brunch today."
interpret DinnerToday    = "Hall is serving dinner today."
interpret MCRDinnerToday = "There’s an MCR dinner tonight!"
interpret NoHallForAges  = "Hall is closed until August 31st."
interpret MaybeHall      = "Who knows. Better get on your bike."

ishallopen :: MonadTime m => m [Message]
ishallopen = do
        currentTime <- getTime
        let today = utctDay currentTime
        pure $ ishallopentoday today

ishallopentoday :: Day -> [Message]
ishallopentoday (subtract 57564 . toModifiedJulianDay -> day)
    | day >= 95 || day < 0  = [MaybeHall]
    | day < 48 || day >= 66 = 
        case day `mod` 7 of
            0 -> [NoHallToday]
            1 -> [DinnerToday]
            _ -> [BreakFastToday, LunchToday]
    | day >= 48 && day < 67 = [NoHallForAges]
    | otherwise             = [MaybeHall]
