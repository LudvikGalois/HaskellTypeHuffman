{-# Language FunctionalDependencies, MultiParamTypeClasses,
    FlexibleInstances, UndecidableInstances, TypeOperators #-}

-- Histograms for use in huffman encoding
-- Ludvik 'Probie' Galois 2014

module Histogram where

import TypePrelude

-- We'll need binary search trees

data BinaryNull
data BinaryNode key val left right 

class BinaryTree a

instance BinaryTree BinaryNull
instance (BinaryTree b, BinaryTree c) => BinaryTree (BinaryNode key a b c)

-- Let's insert values into binary search trees
         
class InsertIntoBinaryTree key val tree newTree | key val tree -> newTree where
  insertIntoBinaryTree :: key -> val -> tree -> newTree
  insertIntoBinaryTree = undefined

instance InsertIntoBinaryTree key val BinaryNull
         (BinaryNode key val BinaryNull BinaryNull)
instance ( LEQ key key' lte, LEQ key' key gte, And lte gte eq, Not eq neq
         , And lte neq lt, And gte neq gt, Is equalp (BinaryNode key val leftTree rightTree)
         , If eq equalp compp res, InsertIntoBinaryTree key val leftTree leftTree'
         , InsertIntoBinaryTree key val rightTree rightTree'
         , If lte (BinaryNode key' val' leftTree' rightTree)
                  (BinaryNode key' val' leftTree rightTree') compp)
            => InsertIntoBinaryTree key val (BinaryNode key' val' leftTree rightTree) res
         
-- And have a specialised version for histograms

class InsertIntoHistogram key tree newTree | key tree -> newTree where
  insertIntoHistogram :: key -> tree -> newTree
  insertIntoHistogram = undefined

type EB1 = EightBit One Zero Zero Zero Zero Zero Zero Zero
  
-- val'' in the absence of val may seem weird.
-- tl;dr I copy pasted my above code
instance (Binary key) => InsertIntoHistogram key BinaryNull
         (BinaryNode key (EightBit One Zero Zero Zero Zero Zero Zero Zero) BinaryNull BinaryNull)
instance ( Binary key, LEQ key key' lte, LEQ key' key gte, And lte gte eq, Not eq neq
         , And lte neq lt, And gte neq gt, Is equalp (BinaryNode key val'' leftTree rightTree)
         , Add val' EB1 val'', If eq equalp compp res, InsertIntoHistogram key leftTree leftTree'
         , InsertIntoHistogram key rightTree rightTree'
         , If lte (BinaryNode key' val' leftTree' rightTree)
                  (BinaryNode key' val' leftTree rightTree')
                   compp)
            => InsertIntoHistogram key (BinaryNode key' val' leftTree rightTree) res

-- Convert a list of bytes to a histogram
         
class ByteListToHistogram a b | a -> b where
  byteListToHistogram :: a -> b
  byteListToHistogram = undefined

instance (ByteListToHistogram' a BinaryNull b) => ByteListToHistogram a b

class ByteListToHistogram' a b c | a b -> c where
  byteListToHistogram' :: a -> b -> c
  byteListToHistogram' = undefined
         
instance ByteListToHistogram' Nil a a
instance (InsertIntoHistogram x tree tree', ByteListToHistogram' xs tree' res)
         => ByteListToHistogram' (x ::: xs) tree res

-- Find a value in a binary search tree
         
class Lookup key tree val | key tree -> val where
  lookup :: key -> tree -> val
  lookup = undefined
         
instance Lookup paths BinaryNull Nil
instance (Lookup key left leftRes, Lookup key right rightRes, LEQ key key' lte,
          LEQ key' key gte, And lte gte eq, If lte leftRes rightRes neq, If eq val' neq res) =>
         Lookup key (BinaryNode key' val' left right) res
  
emptyTree :: BinaryNull
emptyTree = undefined
