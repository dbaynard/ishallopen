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
ishallopenApp = defineControllerView "Is hall open today?" messageStore $ \msg () -> 
    div_ $ do
        p_ [ "class" $= "message" ] . elemText . interpret $ msg
        p_ [
             "class" $= "refresh"
           , onClick $ \_ _ -> dispatch ()
           ] "Refresh"
