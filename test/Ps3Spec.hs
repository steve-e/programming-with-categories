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
  describe "fib" $ do
    it "fib 0" $ do
      fib (Fix Zero) `shouldBe` 1
    it "fib 1" $ do
      fib (Fix ( Succ ( Fix Zero))) `shouldBe` 2
    it "fib 4" $ do
      fib (Fix (Succ (Fix (Succ (Fix (Succ ( Fix Zero))))))) `shouldBe` 5