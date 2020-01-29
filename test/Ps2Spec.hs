module Ps2Spec (main, spec) where


import Test.Hspec
import Ps2

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ConstantBool functor" $ do
    it "Maps two ints to the same object" $ do
      (constantBool True 1) `shouldBe` (constantBool True 2)
    it "fmap maps morphisms to the identity morphism" $ do
      (fmap (+1) (ConstantBool True)::(ConstantBool Int) )`shouldBe` (ConstantBool True)