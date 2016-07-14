{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE JavaScriptFFI #-}

module IsHallOpenToday.Store (
    module IsHallOpenToday.Store
)   where

import React.Flux

import IsHallOpenToday.Common
-- import IsHallOpenToday.Lens

import Data.JSString ()
import GHCJS.Types

{-|
  Print the current date.
-}
foreign import javascript unsafe "$r = new Date().toDateString()"
    js_todayDate :: IO JSString

newtype Store = Store { _unstore :: (Message, JSString) }

-- unstore :: Store :~> (Message, Int)
-- unstore f Store{..} = (\_unstore -> Store{_unstore, ..}) <$> f _unstore

instance StoreData Store where
    type StoreAction Store = ()
    transform () _ = curry Store <$> ishallopen <*> js_todayDate

instance StoreData Day where
    type StoreAction Day = ()
    transform () _ = getToday

messageStore :: ReactStore Message
messageStore = mkStore Checking

dayStore :: ReactStore Day
dayStore = mkStore $ runIdentity getToday
