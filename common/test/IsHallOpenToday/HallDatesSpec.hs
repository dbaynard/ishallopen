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
        it "is closed between April 8th and 18th inclusive" $
            property $ \(x :: ClosedDates) -> (ishallopentoday . coerce) x === NoHallForAges
        it "is open for breakfast and lunch on other weekdays" $
            property $ \(x :: PreSummerDates) ->
                    (\z@(dayOfWeek . coerce -> y) -> coerce z /= (read "2016-07-12" :: Day) && y >= 1 && y <= 5) x
                    ==> (ishallopentoday . coerce) x === BandLToday
        it "is open for breakfast and lunch on other weekdays" $
            property $ \(x :: PostSummerDates) ->
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
   arbitrary = "2017-04-08" `dateRange` "2017-04-18"
instance Arbitrary PreSummerDates where
   arbitrary = "2017-03-23" `dateRange` "2017-04-07"
instance Arbitrary PostSummerDates where
   arbitrary = "2017-04-19" `dateRange` "2017-04-30"

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
