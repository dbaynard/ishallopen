{-# LANGUAGE OverloadedStrings #-}

module BuildSpec (main, spec) where

import Test.Hspec
import Build

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Sources" $ do
        it "should match ghcjs directory" $ do
           undefined 
    describe "Build" $ do
        it "does something" $ do
           undefined 
