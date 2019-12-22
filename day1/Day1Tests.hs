module Day1Tests where

import Day1

import Test.Hspec
import Text.Printf (printf)

testCalculateFuel :: Integer -> Integer -> Spec
testCalculateFuel mass fuel =
  it (printf "should return the mass divided by 3, rounded down and subtracted 2, including the mass of the fuel: %d -> %d \n" mass fuel) $
    calculateFuel mass `shouldBe` fuel


main = hspec $ do
  describe "calculateFuel" $ do
    testCalculateFuel 14 2
    testCalculateFuel 1969 966
    testCalculateFuel 100756 50346
