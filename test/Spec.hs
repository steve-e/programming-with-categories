{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Spec (main, spec) where

import Test.Hspec
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "message" $ do
    it "returns a String" $ do
      message `shouldBe` "foo bar"