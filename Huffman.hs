{-# Language FunctionalDependencies, MultiParamTypeClasses,
    FlexibleInstances, UndecidableInstances, TypeOperators,
    NoMonomorphismRestriction #-}

-- Huffman encoding in the Haskell type system
-- It takes a list of bytes, and gives you a huffman tree
-- and a list of bits
-- Ludvik 'Probie' Galois 2014

module Huffman where

import TypePrelude
import Histogram
import Prelude hiding (and)

-- Frequency trees
  
data Leaf freq elem
data Node freq left right

class FreqTree a

instance (Binary freq) => FreqTree (Leaf freq elem)
instance (Binary freq) => FreqTree (Node freq left right)

class GetFrequency a b | a -> b where
    getFrequency :: a -> b
    getFrequency = undefined

instance (Binary freq) => GetFrequency (Leaf freq b) freq
instance (Binary freq) => GetFrequency (Node freq l r) freq


-- Merge two frequency trees

class Merge a b c | a b -> c where
   merge :: a -> b -> c
   merge = undefined

instance ( GetFrequency t1 a, GetFrequency t2 b, Add a b c
         , FreqTree t1, FreqTree t2) => Merge t1 t2 (Node c t1 t2)

-- A definition of LEQ on frequency trees, using their frequencies    
instance (LEQ aFreq bFreq res) => LEQ (Leaf aFreq x) (Leaf bFreq y) res
instance (LEQ aFreq bFreq res) => LEQ (Leaf aFreq x) (Node bFreq lt rt) res
instance (LEQ aFreq bFreq res) => LEQ (Node aFreq lt rt) (Leaf bFreq y) res
instance (LEQ aFreq bFreq res) =>
         LEQ (Node aFreq xlt xrt) (Node bFreq ylt yrt) res
         
-- We're going to need a sorting algorithm; why not mergsort
class Split a b c | a -> b c where
    split :: a -> (b, c)
    split = undefined

instance Split Nil Nil Nil
instance Split (a ::: Nil) (a ::: Nil) Nil
instance (List c, Split c left right) => Split (a ::: b ::: c) (a ::: left)
                                                               (b ::: right)

class MergeLists a b c | a b -> c where
    mergeLists :: a -> b -> c
    mergeLists = undefined

instance MergeLists Nil y y
instance MergeLists (x ::: xs) Nil (x ::: xs)
instance ( List xs, List ys, LEQ x y p, If p x y newElem
         , If p xs (x ::: xs) newxs, If p (y ::: ys) ys newys
         , MergeLists newxs newys res) => MergeLists (x ::: xs) (y ::: ys)
                                                     (newElem ::: res)

class MergeSort a b | a -> b where
    mergeSort :: a -> b 
    mergeSort = undefined

instance MergeSort Nil Nil
instance MergeSort (a ::: Nil) (a ::: Nil)
instance ( Split (x ::: y ::: zs) l1 l2, MergeSort l1 l1', MergeSort l2 l2'
         , MergeLists l1' l2' res) => MergeSort (x ::: y ::: zs) res

class MergeTrees a b | a -> b where
    mergeTrees :: a -> b
    mergeTrees = undefined

instance (FreqTree a) => MergeTrees (a ::: Nil) a
instance ( MergeSort (x ::: y ::: zs) (h1 ::: h2 ::: t2), Merge h1 h2 resNode
         , MergeTrees (resNode ::: t2) res) => MergeTrees (x ::: y ::: zs) res

-- Get the leaves from a histogram
         
class GetLeaves a b | a -> b where
    getLeaves :: a -> b
    getLeaves = undefined

class GetLeaves' a b c | a b -> c where
    getLeaves' :: a -> b -> c
    getLeaves' = undefined

instance (GetLeaves' x Nil res) => GetLeaves x res
    
instance GetLeaves' BinaryNull acc acc
         
instance (GetLeaves' right acc acc', GetLeaves' left (Leaf val key ::: acc') res)
         => GetLeaves' (BinaryNode key val left right) acc res
         
-- Get a list of pairs containing the path and the element it matches
         
class FreqTreeToPathTree a b | a -> b where
  freqTreeToPathTree :: a -> b
  freqTreeToPathTree = undefined

instance (FreqTreeToPathTree' a Nil b) => FreqTreeToPathTree a b

class FreqTreeToPathTree' a b c | a b -> c where
  freqTreeToPathTree' :: a -> b -> c
  freqTreeToPathTree' = undefined

instance (Reverse acc acc') => FreqTreeToPathTree' (Leaf freq val) acc ((Pair val acc') ::: Nil)
instance (FreqTreeToPathTree' left (Zero ::: acc) left',
          FreqTreeToPathTree' right (One ::: acc) right',
          Concat left' right' res) => FreqTreeToPathTree' (Node freq left right) acc res
          
-- Put them into a binary tree for quicker lookup
         
class PathsToBinaryTree a b | a -> b where
  pathsToBinaryTree :: a -> b
  pathsToBinaryTree = undefined

instance (PathsToBinaryTree' a BinaryNull b) => PathsToBinaryTree a b
         
class PathsToBinaryTree' a acc b | a acc -> b where
  pathsToBinaryTree' :: a -> acc -> b
  pathsToBinaryTree' = undefined

instance PathsToBinaryTree' Nil acc acc
instance (PathsToBinaryTree' xs acc' res, InsertIntoBinaryTree key val acc acc') =>
         PathsToBinaryTree' ((Pair key val) ::: xs) acc res
  
-- Encode bits based on the tree
         
class EncodeBits paths bits encoded | paths bits -> encoded where
  encodeBits :: paths -> bits -> encoded
  encodeBits = undefined

instance EncodeBits a Nil Nil
instance (Lookup x paths x', EncodeBits paths xs res', Concat x' res' res)
         => EncodeBits paths (x ::: xs) res
         
-- And now to encode it

class HuffmanEncode a b | a -> b where
  huffmanEncode :: a -> b
  huffmanEncode = undefined

instance (ByteListToHistogram input histo, GetLeaves histo leaves,
          MergeTrees leaves huffmanTree, FreqTreeToPathTree huffmanTree path,
          PathsToBinaryTree path pathTree, EncodeBits pathTree input compressedBits)
         => HuffmanEncode input (Pair huffmanTree compressedBits)

-- Follow a bit path to a single value, returning the unused bits and the element

class FollowBitPathToValue a b c | a b -> c where
  followBitPathToValue :: a -> b -> c
  followBitPathToValue = undefined

instance FollowBitPathToValue (Leaf freq val) bits (Pair val bits)
instance (FollowBitPathToValue left xs res) => FollowBitPathToValue (Node freq left right) (Zero ::: xs) res
instance (FollowBitPathToValue right xs res) => FollowBitPathToValue (Node freq left right) (One ::: xs) res
         
-- And now decode it
         
class HuffmanDecode tree bits decoded | tree bits -> decoded where
  huffmanDecode :: tree -> bits -> decoded
  huffmanDecode = undefined
         
instance HuffmanDecode a Nil Nil
instance (FollowBitPathToValue tree (x ::: xs) (Pair val bits),
          HuffmanDecode tree bits res) => HuffmanDecode tree (x ::: xs) (val ::: res)

first :: Pair a b -> a
first = undefined
second :: Pair a b -> b
second = undefined
  
testString
  :: EightBit Zero Zero One Zero Zero Zero Zero Zero
     ::: (EightBit Zero Zero Zero Zero Zero Zero Zero Zero
          ::: (EightBit Zero Zero Zero Zero Zero Zero Zero Zero
               ::: (EightBit One Zero Zero Zero Zero Zero Zero Zero
                    ::: (EightBit One Zero Zero Zero Zero Zero Zero Zero
                         ::: (EightBit One Zero Zero Zero Zero Zero Zero Zero
                              ::: (EightBit One Zero Zero Zero Zero Zero Zero Zero
                                   ::: (EightBit Zero Zero One Zero Zero Zero Zero Zero
                                        ::: Nil)))))))

-- load into GHCi
-- try :t <insert one of the values below>

testString = undefined
testHuffman = huffmanEncode testString
testHuffmanTree = first testHuffman
encodedTestString = second testHuffman
decodedTestString = huffmanDecode testHuffmanTree encodedTestString
decodedIsOriginal = (leq testString decodedTestString) `and` (leq decodedTestString testString)
