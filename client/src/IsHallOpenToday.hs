{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IsHallOpenToday (
    react
  , reactStat
)   where

import BasicPrelude

import React.Flux

import IsHallOpenToday.View
import IsHallOpenToday.Dispatcher
import IsHallOpenToday.Store

{-foreign import javascript unsafe "window.alert($1)" js_alert :: JSString -> IO ()-}

react :: String -> IO ()
react text = reactRender text ishallopenApp () <* executeAction (dispatchMessage NoDelay)

reactStat :: IO ()
reactStat = do
        executeAction (dispatchMessage NoDelay)
        putStrLn =<< reactRenderToString False ishallopenApp ()

