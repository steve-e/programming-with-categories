module Ps2 where
import Data.Functor

{-
Question 2. Constant functors.
Let C and D be categories. Given any object d in D, we can define the constant functor Kd : C → D on d. This functor sends every of object C to d ∈ Ob D, and every morphism of C to the identity morphism on d.
(a) Take the set B = {T,F}. Show that the constant functor KB : Set → Set obeys the two functor laws: preservation of composition and preservation of identities.
(b) Implement in Haskell the constant functor on the type Bool
-}

data ConstantBool a = ConstantBool Bool deriving (Eq, Show) 

constantBool :: Bool -> a -> ConstantBool a
constantBool b a = ConstantBool b


instance Functor ConstantBool where
    fmap _ (ConstantBool a) = ConstantBool a

{- 
Question 6. Products in Hask.
Recall that Haskell has a built-in product (pair) type with constructors written:
data (a,b) = (a,b)
Implement isomorphisms of the following type signatures by drawing diagrams and
translating them into code. Explain why the functions you have constructed are isomorphisms.
(a) swap :: (a,b) -> (b,a)
(b) unit :: a -> ((),a)
(c) assoc :: (a,(b,c)) -> ((a,b),c)
-}

-- morphism given in Lecure6
tuple :: (t -> a, t -> b) -> t -> (a, b)
tuple (f,g) = \c -> (f c, g c)

-- point free diagramatic solutions
swap :: (a,b) -> (b,a)
swap = tuple (snd, fst)


unit :: a -> ((), a)
unit = tuple ( const (), id)

assoc :: (a,(b,c)) -> ((a,b),c)
assoc = tuple ( tuple (fst, fst.snd), snd.snd)

-- pattern match solutions
swap2 :: (a,b) -> (b,a)
swap2 (a , b) = (b , a)

unit2 :: a -> ((), a)
unit2 a = ((),a)

assoc2 :: (a,(b,c)) -> ((a,b),c)
assoc2 (a,(b,c)) =  ((a,b),c)
