{-# LANGUAGE OverloadedStrings #-}

module Main where

import IsHallOpenToday.Html

main :: IO ()
main = renderIndex "ishallopenApp"
