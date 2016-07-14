{-# LANGUAGE OverloadedStrings #-}

module IsHallOpenToday.Dispatcher (
    module IsHallOpenToday.Dispatcher
)   where

import React.Flux

import IsHallOpenToday.Store

dispatch :: ViewEventHandler
dispatch = [dispatchDay]

dispatchDay :: SomeStoreAction
dispatchDay = SomeStoreAction dayStore ()
