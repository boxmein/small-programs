module DayOneSpec (spec) where 

import Test.Hspec

import DayOne (dayOnePartOne, dayOnePartTwo, getFirstAndLastDigitPartTwo, firstDigitPartTwo, lastDigitPartTwo)

spec :: Spec
spec = do 
  describe "dayOnePartOne" $ do 
    it "works on test data" $ do 
      dayOnePartOne [
        "1abc2",
        "pqr3stu8vwx",
        "a1b2c3d4e5f",
        "treb7uchet"] `shouldBe` 142
  describe "dayOnePartTwo" $ do 
    it "works on test data" $ do 
      dayOnePartTwo [
        "two1nine",
        "eightwothree",
        "abcone2threexyz",
        "xtwone3four",
        "4nineeightseven2",
        "zoneight234",
        "7pqrstsixteen"] `shouldBe` 281
  describe "getFirstAndLastDigitPartTwo" $ do
    it "works on test data" $ do 
      getFirstAndLastDigitPartTwo "two1nine" `shouldBe` Just "29" 
      getFirstAndLastDigitPartTwo "eightwothree" `shouldBe` Just "83" 
      getFirstAndLastDigitPartTwo "abcone2threexyz" `shouldBe` Just "13" 
      getFirstAndLastDigitPartTwo "xtwone3four" `shouldBe` Just "24" 
      getFirstAndLastDigitPartTwo "4nineeightseven2" `shouldBe` Just "42" 
      getFirstAndLastDigitPartTwo "zoneight234" `shouldBe` Just "14" 
      getFirstAndLastDigitPartTwo "7pqrstsixteen" `shouldBe` Just "76" 
  describe "firstDigitPartTwo" $ do
    it "finds text 'one' before digit 2" $ do 
      firstDigitPartTwo "abcone2threexyz" `shouldBe` Just '1'
  describe "lastDigitPartTwo" $ do
    it "finds text 'three' before digit 2" $ do 
      lastDigitPartTwo "abcone2threexyz" `shouldBe` Just '3'
