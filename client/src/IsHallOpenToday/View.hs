{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IsHallOpenToday.View (
    module IsHallOpenToday.View
)   where

import BasicPrelude

import React.Flux
import Data.JSString ()

import IsHallOpenToday.Store
import IsHallOpenToday.Dispatcher
import IsHallOpenToday.Common

ishallopenApp :: ReactView ()
ishallopenApp = defineControllerView "Is hall open today?" messageStore $ \(Store (msg, date)) () ->
    div_ $ do
        p_ [ classNames [("message",True)]] . elemText . interpret $ msg
        p_ [ classNames [("date",True)]] . elemJSString $ date
        p_ [
             classNames [("refresh",True),("js-only",True)]
           , onClick $ \_ _ -> dispatch Delay
           ] "Refresh"
        noscript_ $
            p_ [ classNames [("warning",True)]] "This site requires javascript. It does some calculations to work out whether hall is open, and that needs javascript. So turn on javascript."
