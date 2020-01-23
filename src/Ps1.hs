{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
module Ps1 
    ( q1a,
      q1b,
      Category(..),
      Mor(..),
      Two(..)
    ) where

import Control.Arrow
import Data.Monoid
{-
Question 1. Functions in mathematics and Haskell.
Suppose f : Int → Int sends an integer to its square, f(x) := x2 and that g: Int →
Int sends an integer to its successor, g(x) := x + 1.
(a) Write f and g in Haskell, including their type signature and their implementation. (b) Leth:=f◦g. Whatish(2)?
(c) Leti:=f􏰀g. Whatisi(2)?
-}
q1a::Int
q1a = h 2

q1b::Int
q1b = i 2

h::Int -> Int
h = f . g 

i::Int -> Int
i = f >>> g

f::Int -> Int
f x = x^2

g::Int -> Int
g = \x -> x +1



{-
Question 9. Defining a toy category in Haskell.
-}

class Category obj mor | mor -> obj where 
    dom :: mor -> obj
    cod :: mor -> obj
    idy :: obj -> mor
    cmp :: mor -> mor -> Maybe mor

data Mor = Id Two |AB deriving (Eq, Show)
data Two = A|B deriving (Eq, Show)

instance Category Two Mor where
    dom AB = A
    dom (Id a) = a
    cod AB = B
    cod (Id a) = a
    idy a = Id a
    cmp (Id x) (Id y) 
        | x == y = Just (Id x)   
    cmp AB (Id B) = Just AB
    cmp (Id A) AB = Just AB
    cmp _ _ = Nothing  

    
