{-# LANGUAGE OverloadedStrings #-}

module IsHallOpenToday.Dispatcher (
    module IsHallOpenToday.Dispatcher
)   where

import React.Flux

import IsHallOpenToday.Store

dispatch :: () -> [SomeStoreAction]
dispatch () = [SomeStoreAction messageStore ()]
