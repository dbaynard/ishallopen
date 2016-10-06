{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module IsHallOpenToday.HallDatesSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Control.Monad
import Control.Arrow
import Data.Coerce
import Data.Maybe (fromMaybe)

import Data.Thyme
import Data.Thyme.Calendar.WeekDate
import System.Locale

import IsHallOpenToday.HallDates
import IsHallOpenToday.Lens

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Hall open status" $ do
        it "is closed between August 12th and 31st inclusive" $
            property $ \(x :: ClosedDates) -> (ishallopentoday . coerce) x === NoHallForAges
        it "is closed on other Saturdays before 29th September" $
            property $ \(x :: SummerDates) -> (dayOfWeek . coerce) x == 6 ==> (ishallopentoday . coerce) x === NoHallToday
        it "is open for dinner on other Sundays before 12th August" $
            property $ \(x :: PreSummerDates) -> (dayOfWeek . coerce) x == 7 ==> (ishallopentoday . coerce) x === DinnerToday
        it "is closed for on other Sundays before 29th September" $
            property $ \(x :: PostSummerDates) -> (dayOfWeek . coerce) x == 7 ==> (ishallopentoday . coerce) x === NoHallToday
        it "is only serving brunch on 2016-10-02" $
            ishallopentoday (read "2016-10-02") === BrunchToday
        it "reads MCR dinner when there are MCR dinners" $
            ishallopentoday (read "2016-07-12") === MCRDinnerToday
        it "is open for breakfast and lunch on other weekdays before 29th September" $
            property $ \(x :: SummerDates) ->
                    (\z@(dayOfWeek . coerce -> y) -> coerce z /= (read "2016-07-12" :: Day) && y >= 1 && y <= 5) x
                    ==> (ishallopentoday . coerce) x === BandLToday

newtype ClosedDates = ClosedDates Day
    deriving (Show)
newtype SummerDates = SummerDates Day 
    deriving (Show)
newtype PreSummerDates = PreSummerDates Day 
    deriving (Show)
newtype PostSummerDates = PostSummerDates Day
    deriving (Show)

instance Arbitrary ClosedDates where
   arbitrary = "2016-08-12" `dateRange` "2016-08-31"
instance Arbitrary PreSummerDates where
   arbitrary = "2016-06-25" `dateRange` "2016-08-11"
instance Arbitrary PostSummerDates where
   arbitrary = "2016-09-01" `dateRange` "2016-09-28"
instance Arbitrary SummerDates where
   arbitrary = oneof [
                       "2016-06-25" `dateRange` "2016-08-11"
                     , "2016-09-01" `dateRange` "2016-09-28"
                     ]

dayOfWeek :: Day -> DayOfWeek
dayOfWeek = view (weekDate . _wdDay)

dateRange :: Coercible Day a
          => String -- ^ From date
          -> String -- ^ To date
          -> Gen a
dateRange = curry $ coerce . choose . join (***) readDay
infix 2 `dateRange`

parseDay :: String -> Maybe Day
parseDay = parseTime ukTimeLocale "%Y-%m-%d"

readDay :: String -> Day
readDay = fromMaybe (error "Can’t read day") . parseDay

mzip :: Monad m => m a -> m b -> m (a, b)
mzip = curry $ runKleisli (Kleisli id *** Kleisli id)
{-# INLINE mzip #-}

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = fmap fst &&& fmap snd
{-# INLINE funzip #-}

ukTimeLocale :: TimeLocale
ukTimeLocale = defaultTimeLocale { dateFmt = "%d/%m/%Y" }
