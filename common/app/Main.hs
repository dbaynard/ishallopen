{-# LANGUAGE OverloadedStrings #-}

module Main where

import IsHallOpenToday.Common
import IsHallOpenToday.Html

main :: IO ()
main = renderIndex "ishallopenApp"
