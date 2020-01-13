module Ps1 
    ( q1a,
      q1b
    ) where

import Control.Arrow
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






