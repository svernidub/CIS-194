{-# OPTIONS_GHC -Wall #-}

{-|
Module      : BinaryTree
Description : CIS194 homework #4
Copyright   : (c) Sergey Vernidub 2018
License     : GPL-3
Maintainer  : svernidub@cropio.com
Stability   : experimental
Portability : POSIX

Contains definition of balanced binary tree
-}

module BinaryTree (
    Tree(..),
    foldTree,
    isBalanced
) where

import           Data.List (foldl')

-- * Types

-- | Balanced Binary tree
data Tree a = Leaf                             -- ^ Empty Tree node
            | Node Integer (Tree a) a (Tree a) -- ^ Node of a tree
            deriving (Show, Eq)


-- * Functions

-- | Insert list of keys to binary tree
foldTree :: Ord a
         => [a]    -- ^ List of keys
         -> Tree a -- ^ Resulting balanced tree
foldTree = foldl' insert Leaf


-- | Checks if tree is balanced
--
--   Tree is balanced if the height of its left and right subtrees
--   differ by no more than 1, and its left and right subtrees are also balanced
isBalanced :: Tree a -- ^ Binary tree
           -> Bool   -- ^ was input tree balanced
isBalanced Leaf                       = True
isBalanced node@(Node _ left _ right) = thisIsBalanced && leftIsBalanced && rightIsBalanced
    where
        thisIsBalanced  = (abs . bfactor) node <= 1
        leftIsBalanced  = isBalanced left
        rightIsBalanced = isBalanced right


-- | Inserts node with specified key in tree, fixes its height and rebalances it
insert :: Ord a
       => Tree a -- ^ input tree
       -> a      -- ^ key
       -> Tree a -- ^ tree with inserted key
insert Leaf                   k = Node 0 Leaf k Leaf
insert (Node _ left k' right) k
    | k < k' = balance $ Node 0 (insert left k) k' right
    | k > k' = balance $ Node 0 left k' (insert right k)
insert _                      _ = Leaf


height :: Tree a -> Integer
height Leaf           = 0
height (Node n _ _ _) = n


bfactor :: Tree a -> Integer
bfactor Leaf                  = 0
bfactor (Node _ left _ right) = height right - height left


rotateRight :: Tree a -> Tree a
rotateRight Leaf                  = Leaf
rotateRight (Node _ left k right) = let
                                        (Node _ qleft qk qright) = left
                                        p = fixHeight $ Node 0 qright k right
                                    in
                                        fixHeight $ Node 0 qleft qk p


rotateLeft :: Tree a -> Tree a
rotateLeft Leaf                  = Leaf
rotateLeft (Node _ left k right) = let
                                       (Node _ pleft pk pright) = right
                                       q = fixHeight $ Node 0 left k pleft
                                   in
                                       fixHeight $ Node 0 q pk pright


balance :: Tree a -> Tree a
balance Leaf = Leaf
balance node = balanceBfactorMinus2 . balanceBfactorPlus2 . fixHeight $ node


balanceBfactorMinus2 :: Tree a -> Tree a
balanceBfactorMinus2 p
    | bfactor p /= 2 = p
    | otherwise      = balanceBfactorMinus2' p
        where
            balanceBfactorMinus2' Leaf = Leaf
            balanceBfactorMinus2' pp@(Node n left k right)
                | bfactor right < 0 = Node n left k (rotateRight right)
                | otherwise         = rotateLeft pp


balanceBfactorPlus2 :: Tree a -> Tree a
balanceBfactorPlus2 p
    | bfactor p /= -2 = p
    | otherwise       = balanceBfactorPlus2' p
        where
            balanceBfactorPlus2' Leaf = Leaf
            balanceBfactorPlus2' pp@(Node n left k right)
                | bfactor left > 0 = Node n (rotateLeft left) k right
                | otherwise        = rotateRight pp


fixHeight :: Tree a -> Tree a
fixHeight Leaf                                           = Leaf
fixHeight (Node _ l@Leaf            k r@Leaf           ) = Node 0 l k r
fixHeight (Node _ l@(Node lh _ _ _) k r@Leaf           ) = Node (lh + 1) l k r
fixHeight (Node _ l@Leaf            k r@(Node rh _ _ _)) = Node (rh + 1) l k r
fixHeight (Node _ l@(Node lh _ _ _) k r@(Node rh _ _ _)) = Node (1 + max lh rh) l k r
