module Day2Tests where
import Day2

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
  describe "changeElement" $ do
    it "returns a list with the new value at the specified index" $
      changeElement 0 10 [1] `shouldBe` [10]
    it "returns an empty list when provided and empty list" $
      changeElement 0 10 [] `shouldBe` []
    it "returns an empty list when index is < 0" $
      changeElement (-1) 10 [1] `shouldBe` []
    it "returns an empty list when index is greater than the length" $
      changeElement 2 10 [1] `shouldBe` []

  describe "compute" $ do
    it "should add the numbers at the positions listed after the `1` and store then at the position after that" $
      compute [1,0,0,0,99] 0 `shouldBe` [2,0,0,0,99]
    it "should multiply the numbers at the positions listed after the `2` and store then at the position after that" $
      compute [2,3,0,3,99] 0 `shouldBe` [2,3,0,6,99]
    it "should return the list if the number at the cursor is anything else" $
      compute [99,0,0,0] 0 `shouldBe` [99,0,0,0]

  describe "intCodeProgram" $ do
    it (printf "keeps running, moving forward 4 each time, until it reaches a code 99") $
      intCodeProgram 0 [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]
