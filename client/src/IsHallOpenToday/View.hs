{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IsHallOpenToday.View (
    module IsHallOpenToday.View
)   where

import BasicPrelude

import React.Flux
import Data.JSString

import IsHallOpenToday.Store
import IsHallOpenToday.Dispatcher
import IsHallOpenToday.Common

ishallopenApp :: JSString -- ^ The date
              -> ReactView ()
ishallopenApp dateString = defineControllerView "Is hall open today?" messageStore $ \msg () ->
    div_ $ do
        p_ [ classNames [("message",True)]] . elemText . interpret $ msg
        p_ [ classNames [("date",True)]] . elemJSString $ dateString
        p_ [
             classNames [("refresh",True)]
           , onClick $ \_ _ -> dispatch
           ] "Refresh"
