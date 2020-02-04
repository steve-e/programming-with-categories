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


{-
Question 3. Naturals.
Natural numbers can be represented in Haskell as a recursive data structure
data Nat = Zero | Succ Nat
(a) Implement a type Nat2, isomorphic to Nat, but this time defined as an initial
algebra of a functor.
(b) Using a catamorphism, define a function Nat2 -> Int that maps n to the n-th
Fibonacci number.
(c) Define a coalgebra whose anamorphism is a (partial) function Int -> Nat2 that
sends a non-negative Int into its fixed-point representation (ie. its representation
as a value of Nat2).
-}

data Nat2 n = Zero | Succ n deriving (Functor, Show, Eq)
type NatInt = Fix Nat2 

intAlg :: Nat2 Int-> Int
intAlg Zero  = 0
intAlg (Succ n) = n + 1

toInt::NatInt-> Int
toInt n = cata intAlg n


fibAlg :: Nat2 (Int, Int)-> (Int,Int)
fibAlg Zero   = (0 , 1)
fibAlg (Succ (n1,n2)) = (n2, n1 + n2)

fib::NatInt -> Int
fib n = fst  (fib2 n)
fib2 n = cata fibAlg n


natAlg::Int -> Nat2 Int
natAlg 0 = Zero
natAlg n = Succ(n-1)


toNat::Int -> NatInt
toNat n = ana natAlg n 





data Cont s x = Cont ((x -> s) -> s)

instance Functor (Cont s) where 
    fmap f (Cont ass) = Cont (\bs -> ass (bs . f))

