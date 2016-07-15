{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IsHallOpenToday.Css (
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
                ".message" & do
                    fontSize $ pt 32
            (p <> a) # ".refresh" ? do
                margin nil auto nil auto
                fontSize $ pt 10
                width (em 6)
                padding (ex 1) (em 1) (ex 1) (em 1)
                textDecoration none
                link &
                    color steelblue
                hover & do
                    transition "backgroundColor" (sec 1.5) ease (sec 0)
                    color azure
                    backgroundColor steelblue
                active & do
                    transition "backgroundColor" (sec 1.5) ease (sec 0)
                    color azure
                    backgroundColor steelblue
                visited & do
                    color steelblue
                    hover & do
                        transition "backgroundColor" (sec 1.5) ease (sec 0)
                        color azure
                        backgroundColor steelblue
