{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import Test.Hspec

import Lib (stringReplace)

main :: IO ()
main = hspec $ do
  describe "stringReplace" $ do
    it "replaces single chars" $ do 
      stringReplace "babababa" "a" "c" `shouldBe` ("bcbababa" :: String)

