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
