module Ps3Spec (main, spec) where


import Test.Hspec
import Data.Fix
import Ps3

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ListInt product" $ do
    it "product of 3,2,3" $ do
      productF (Fix (Cons 3 $ Fix(Cons 2 $ Fix(Cons 3 $ Fix Nil)))) `shouldBe` 18
  describe "toInt" $ do
    it "toInt 0" $ do
      toInt (Fix Zero) `shouldBe` 0
    it "toInt 1" $ do
      toInt (Fix ( Succ ( Fix Zero))) `shouldBe` 1
    it "toInt 2" $ do
      toInt (Fix (Succ (Fix (Succ ( Fix Zero))))) `shouldBe` 2
  describe "toNat" $ do
    it "toNat 0" $ do
      toNat 0  `shouldBe` (Fix Zero)
    it "toNat 1" $ do
      toNat 1`shouldBe` (Fix ( Succ ( Fix Zero))) 
    it "toNat 2" $ do
      toNat 2 `shouldBe`   (Fix (Succ (Fix (Succ ( Fix Zero)))))     
  describe "fib" $ do
    it "fib 0 is 0" $ do
      fib (Fix Zero) `shouldBe` 0
    it "fib 1 is 1" $ do
      fib (Fix ( Succ ( Fix Zero))) `shouldBe` 1
    it "fib 2 is 1" $ do
      fib (toNat 2) `shouldBe` 1
    it "fib 6" $ do
      fib (toNat 6) `shouldBe` 8