{-# LANGUAGE OverloadedStrings #-}

module IsHallOpenToday.Dispatcher (
    module IsHallOpenToday.Dispatcher
)   where

import React.Flux

import IsHallOpenToday.Store

dispatch :: Delay -> ViewEventHandler
dispatch delay = [dispatchMessage delay]

dispatchMessage :: Delay -> SomeStoreAction
dispatchMessage = SomeStoreAction messageStore
