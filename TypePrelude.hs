{-# Language FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances
  , UndecidableInstances, TypeOperators #-}

-- A simple Prelude for writing code in the Haskell type system
-- Ludvik 'Probie' Galois 2014

module TypePrelude where

-- A few basic types

data Pair a b

data Nil

-- ::: is : for types
data a ::: b

infixr 5 :::

class List a where

-- In function form for testing
  
cons :: a -> b -> (a ::: b)
cons = undefined

nil :: Nil
nil = undefined

instance List Nil
instance (List b) => List (a ::: b)

class Uncons a b c | a -> b c where
    uncons :: a -> (b,c)
    uncons = undefined

instance (List b) => Uncons (a ::: b) a b

-- Reverse a list
class Reverse a b | a -> b where
  reverse :: a -> b
  reverse = undefined

instance (Reverse' a Nil b) => Reverse a b

class Reverse' a acc b | a acc -> b where
  reverse' :: a -> acc -> b
  reverse' = undefined

instance Reverse' Nil acc acc
instance (Reverse' xs (x ::: acc) res) => Reverse' (x ::: xs) acc res
         
-- Concat is ++ for type lists
class Concat a b c | a b -> c where
  concat :: a -> b -> c
  concat = undefined

instance Concat Nil b b
instance (Concat xs ys ys') => Concat (x ::: xs) ys (x ::: ys')
         
-- Is is kind of like =
-- it's handy to have
class Is a b | a -> b where
  is :: a -> b
  is = undefined

instance Is a a

-- We need bits
         
data Zero
data One

class Bit a

instance Bit Zero
instance Bit One

-- And possibly nats

data Z
data S a

class Nat a where
    s :: a -> S a
    s = undefined

instance Nat Z
instance (Nat a) => Nat (S a)

class Binary a

instance (Bit a, Bit b, Bit c, Bit d, Bit e, Bit f, Bit g, Bit h)
         => Binary (EightBit a b c d e f g h)
         
-- And booleans that are separate from bits

data True
data False 

-- Conditional branching(ish)
  
class If p x y z | p x y -> z where
    choice :: p -> x -> y -> z
    choice = undefined

instance If True x y x
instance If False x y y

-- Boolean and

class And a b c | a b -> c where
    and :: a -> b -> c
    and = undefined

instance And True y y
instance And False y False
  
-- LEQ is <=

class LEQ a b c | a b -> c where
   leq :: a -> b -> c
   leq = undefined

instance (Nat a) => LEQ Z a True
instance (Nat a) => LEQ (S a) Z False
instance (Nat a, Nat b, LEQ a b c) => LEQ (S a) (S b) c

instance LEQ Zero Zero True
instance LEQ Zero One True
instance LEQ One Zero False
instance LEQ One One True
         
class Add a b c | a b -> c where
   add :: a -> b -> c
   add = undefined

instance (Nat b) => Add Z b b
instance (Nat a) => Add a Z a
instance (Nat a, Nat b, Add a b c) => Add (S a) (S b) (S (S c))

-- Logic operations are useful

class Not a b | a -> b where
  not :: a -> b
  not = undefined

instance Not Zero One
instance Not One Zero
instance Not True False
instance Not False True

class LAnd a b c | a b -> c where
  land :: a -> b -> c
  land = undefined

instance LAnd One b b
instance LAnd Zero b Zero
  
class LOr a b c | a b -> c where
  lor :: a -> b -> c
  lor = undefined

instance LOr One b One
instance LOr Zero b b

class LXor a b c | a b -> c where
  lxor :: a -> b -> c
  lxor = undefined

instance LXor One One Zero
instance LXor One Zero One
instance LXor Zero One One
instance LXor Zero Zero Zero

-- Maths is hard

data EightBit a b c d e f g h
         
class FullAdder a b cin s cout | a b cin -> s cout where
  fullAdder :: a -> b -> cin -> (s, cout)
  fullAdder = undefined

instance ( LXor a b axorb, LXor axorb cin s, LAnd a b aandb, LAnd axorb cin carry
         , LOr aandb carry cout) => FullAdder a b cin s cout
  
-- Let's have some 8-bit ints
instance ( LXor x0 y0 z0, LAnd x0 y0 c0
         , FullAdder x1 y1 c0 z1 c1
         , FullAdder x2 y2 c1 z2 c2
         , FullAdder x3 y3 c2 z3 c3
         , FullAdder x4 y4 c3 z4 c4
         , FullAdder x5 y5 c4 z5 c5
         , FullAdder x6 y6 c5 z6 c6
         , FullAdder x7 y7 c6 z7 c7) => 
         Add (EightBit x0 x1 x2 x3 x4 x5 x6 x7)
             (EightBit y0 y1 y2 y3 y4 y5 y6 y7)
             (EightBit z0 z1 z2 z3 z4 z5 z6 z7)

-- Let's have a terrible definition of LEQ
         
data EQ_T
data LT_T
data GT_T

class LEQBIT a b c | a b -> c

instance LEQBIT Zero Zero EQ_T
instance LEQBIT Zero One LT_T
instance LEQBIT One Zero GT_T
instance LEQBIT One One EQ_T

class RollDownLEQ a b c | a b -> c
instance RollDownLEQ EQ_T GT_T GT_T
instance RollDownLEQ EQ_T LT_T LT_T
instance RollDownLEQ EQ_T EQ_T EQ_T
instance RollDownLEQ GT_T a GT_T
instance RollDownLEQ LT_T a LT_T

class RollDownLEQConv a b | a -> b
instance RollDownLEQConv EQ_T True
instance RollDownLEQConv GT_T False
instance RollDownLEQConv LT_T True
         
instance ( LEQBIT x7 y7 r7, LEQBIT x6 y6 r6, LEQBIT x5 y5 r5, LEQBIT x4 y4 r4, LEQBIT x3 y3 r3
         , LEQBIT x2 y2 r2, LEQBIT x1 y1 r1, LEQBIT x0 y0 r0, RollDownLEQ r7 r6 r6'
         , RollDownLEQ r6' r5 r5', RollDownLEQ r5' r4 r4', RollDownLEQ r4' r3 r3'
         , RollDownLEQ r3' r2 r2', RollDownLEQ r2' r1 r1', RollDownLEQ r1' r0 r0',
           RollDownLEQConv r0' res) =>
         LEQ (EightBit x0 x1 x2 x3 x4 x5 x6 x7) (EightBit y0 y1 y2 y3 y4 y5 y6 y7) res

-- In retrospect, negating, adding and then checking the first bit would
-- probably have been easier
