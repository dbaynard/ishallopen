{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module HallDatesSpec (
    module HallDatesSpec
)   where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Control.Monad
import Control.Arrow
import Data.Coerce

import HallDates
import Data.Thyme
import System.Locale

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Hall open status" $ do
        it "Is closed between August 12th and 30th inclusive" $ do
            property $ \(x :: ClosedDates) -> (ishallopentoday . coerce) x == NoHallForAges

newtype ClosedDates = ClosedDates Day
    deriving (Show)

instance Arbitrary ClosedDates where
   arbitrary = "2016-08-12" `dateRange` "2016-08-30"

dateRange :: Coercible Day a
          => String -- ^ From date
          -> String -- ^ To date
          -> Gen a
dateRange = curry $ coerce . choose . join (***) readDay
infix 2 `dateRange`

parseDay :: String -> Maybe Day
parseDay = parseTime ukTimeLocale "%Y-%m-%d"

readDay :: String -> Day
readDay = maybe (error "Can’t read day") id . parseDay

mzip :: Monad m => m a -> m b -> m (a, b)
mzip = curry $ runKleisli (Kleisli id *** Kleisli id)
{-# INLINE mzip #-}

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = fmap fst &&& fmap snd
{-# INLINE funzip #-}

ukTimeLocale :: TimeLocale
ukTimeLocale = defaultTimeLocale { dateFmt = "%d/%m/%Y" }
