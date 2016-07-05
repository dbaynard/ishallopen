{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Css (
    css
  , renderStrict
)   where

import BasicPrelude
import Clay
import Data.String.Conv

renderStrict :: Css -> Text
renderStrict = toS . render

css :: Css
css = -- do
        body ? do
            backgroundColor azure
            color steelblue
            textAlign $ alignSide sideCenter
            -- textAlign center
            p ? do
                fontFamily ["Helvetica Neue", "Helvetica", "Arial"] [sansSerif]
                fontSize $ pt 32
