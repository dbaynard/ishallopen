{-# LANGUAGE OverloadedStrings #-}

module IsHallOpenToday (
    main
)   where

{-import Data.JSString ()-}
{-import GHCJS.Types-}

import React.Flux

import IsHallOpenToday.View

{-foreign import javascript unsafe "window.alert($1)" js_alert :: JSString -> IO ()-}

main :: IO ()
main = reactRender "ishallopenApp" ishallopenApp ()
