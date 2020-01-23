module Ps1Spec (main, spec) where


import Test.Hspec
import Ps1

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "category 2" $ do
    it "composes AB with Id B" $ do
      cmp AB (Id B) `shouldBe` Just AB
    it "composes Id A with AB" $ do
      cmp (Id A) AB `shouldBe` Just AB
    it "does not compose AB with Id A" $ do
      cmp AB (Id A) `shouldBe` Nothing
    it "composes Id A with Id A" $ do
      cmp (Id A) (Id A) `shouldBe` Just (Id A)
    it "does not compose Id B with Id A" $ do
      cmp (Id B) (Id A) `shouldBe` Nothing