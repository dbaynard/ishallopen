{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IsHallOpenToday.Css (
    css
  , noScriptCss
  , renderStrict
)   where

import BasicPrelude
import Clay
import Data.String.Conv

renderStrict :: Css -> Text
renderStrict = toS . render

noScriptCss :: Css
noScriptCss = ".js-only" ? display none

css :: Css
css = -- do
        body ? do
            backgroundColor azure
            color steelblue
            textAlign $ alignSide sideCenter
            -- textAlign center
            p ? do
                fontFamily ["Helvetica Neue", "Helvetica", "Arial"] [sansSerif]
                ".message" & do
                    fontSize $ pt 32
                ".warning" & do
                    fontSize $ pt 20
                    margin nil auto nil auto
                    padding (ex 1) (em 1) (ex 1) (em 1)
            (p <> a) # ".refresh" ? do
                margin nil auto nil auto
                cursor pointer
                fontSize $ pt 10
                width (em 6)
                padding (ex 1) (em 1) (ex 1) (em 1)
                textDecoration none
                transition "all" (sec 0.5) ease (sec 0)
                link &
                    color steelblue
                hover & linkStyle
                active & linkStyle
                visited & do
                    color steelblue
                    hover & linkStyle
        where
            linkStyle = do
                transition "all" (sec 0.5) ease (sec 0)
                color azure
                backgroundColor steelblue
