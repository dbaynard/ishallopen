{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE JavaScriptFFI #-}

module IsHallOpenToday (
    react
)   where

import Data.JSString ()
import GHCJS.Types

import BasicPrelude

import React.Flux

import IsHallOpenToday.View
import IsHallOpenToday.Dispatcher

{-foreign import javascript unsafe "window.alert($1)" js_alert :: JSString -> IO ()-}

foreign import javascript unsafe "$r = new Date().toDateString()"
    js_todayDate :: IO JSString

react :: String -> IO ()
react text = do
        app <- reactApp
        reactRender text app ()
        executeAction dispatchMessage

reactApp :: IO (ReactView ())
reactApp = ishallopenApp <$> js_todayDate
