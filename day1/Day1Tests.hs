module Day1Tests where

import Day1

import Test.Hspec
import Text.Printf (printf)

testCalculateFuel :: Int -> Int -> Spec
testCalculateFuel mass fuel =
  it (printf "should return the mass divided by 3, rounded down and subtracted 2: %d -> %d \n" mass fuel) $
    calculateFuel mass `shouldBe` fuel


main = hspec $ do
  describe "calculateFuel" $ do
    testCalculateFuel 12 2
    testCalculateFuel 14 2
    testCalculateFuel 1969 654
    testCalculateFuel 100756 33583
