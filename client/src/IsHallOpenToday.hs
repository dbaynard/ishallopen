{-# LANGUAGE OverloadedStrings #-}

module IsHallOpenToday (
    react
)   where

{-import Data.JSString ()-}
{-import GHCJS.Types-}

import React.Flux

import IsHallOpenToday.View

{-foreign import javascript unsafe "window.alert($1)" js_alert :: JSString -> IO ()-}

react :: String -> IO ()
react text = reactRender text ishallopenApp ()
