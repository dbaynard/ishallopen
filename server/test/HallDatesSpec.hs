module HallDatesSpec (
    module HallDatesSpec
)   where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Absurd" $ do
        it "is absurd!" $ do
            2 + 2 `shouldBe` (4 :: Int)
