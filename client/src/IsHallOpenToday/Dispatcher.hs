{-# LANGUAGE OverloadedStrings #-}

module IsHallOpenToday.Dispatcher (
    module IsHallOpenToday.Dispatcher
)   where

import React.Flux

import IsHallOpenToday.Store

dispatch :: ViewEventHandler
dispatch = [dispatchMessage, dispatchDay]

dispatchMessage :: SomeStoreAction
dispatchMessage = SomeStoreAction messageStore ()

dispatchDay :: SomeStoreAction
dispatchDay = SomeStoreAction dayStore ()
