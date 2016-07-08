{-# LANGUAGE NoImplicitPrelude #-}

module IsHallOpenToday.Common (
    module IsHallOpenToday.Common
  , module X
)   where

import BasicPrelude

import IsHallOpenToday.HallDates as X

hallQ :: IO ()
hallQ = putStrLn . interpret =<< ishallopen
