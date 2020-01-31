{-# LANGUAGE DeriveFunctor #-}
module Ps3 where
import Data.Functor
import Data.Fix


{-
Question 2. Catamorphisms. Consider the functor F:
 data F a = Nil | Cons Int a
   deriving Functor
We define the recursive type:
 type ListInt = Fix F
(a) Let isEven :: Int -> Bool take an integer to True if it is even, and False
otherwise. Here is an F-algebra.
hello :: F Bool -> Bool
hello Nil = False
hello Cons n a = isEven n || a
What is the induced catamorphism cata hello :: ListInt -> Bool?
(b) Implement the function product :: ListInt -> Int that takes a list of integers
and returns their product.
-}

data F a = Nil | Cons Int a
    deriving Functor

type ListInt = Fix F

productAlg :: F Int -> Int
productAlg Nil = 1
productAlg (Cons n a) = n * a

productF :: ListInt -> Int
productF = cata productAlg

