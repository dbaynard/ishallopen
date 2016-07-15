{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module IsHallOpenToday.Store (
    module IsHallOpenToday.Store
)   where

import BasicPrelude

import React.Flux

import Control.Concurrent
import Control.DeepSeq
import GHC.Generics

import IsHallOpenToday.Common
-- import IsHallOpenToday.Lens

import Data.JSString ()
import GHCJS.Types

{-|
  Print the current date.
-}
foreign import javascript unsafe "$r = new Date().toDateString()"
    js_todayDate :: IO JSString

data Delay = NoDelay
           | Delay
           deriving (Eq, Ord, Enum, Bounded, Generic, NFData)

newtype Store = Store { _unstore :: (Message, JSString) }

initStore :: Store
initStore = Store (Checking, "…")

-- unstore :: Store :~> (Message, Int)
-- unstore f Store{..} = (\_unstore -> Store{_unstore, ..}) <$> f _unstore

instance StoreData Store where
    type StoreAction Store = Delay
    transform NoDelay _ = curry Store <$> ishallopen <*> js_todayDate
    transform Delay _ = do
        void . forkIO $ do
            threadDelay 1500000
            alterStore messageStore NoDelay
        pure initStore

messageStore :: ReactStore Store
messageStore = mkStore initStore
