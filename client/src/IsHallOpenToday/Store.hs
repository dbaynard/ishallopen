{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module IsHallOpenToday.Store (
    module IsHallOpenToday.Store
)   where

import React.Flux

import IsHallOpenToday.Common

instance StoreData Message where
    type StoreAction Message = ()
    transform () _ = ishallopen

messageStore :: ReactStore Message
messageStore = mkStore MaybeHall
