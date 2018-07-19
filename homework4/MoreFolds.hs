{-# OPTIONS_GHC -Wall #-}

{-|
Module      : MoreFolds
Description : CIS194 homework #4
Copyright   : (c) Sergey Vernidub 2018
License     : GPL-3
Maintainer  : svernidub@cropio.com
Stability   : experimental
Portability : POSIX

Some functions implemented with folds
-}

module MoreFolds (
    xor,
    map',
    myFoldl,
    sieveSundaram
) where

import           Data.List (foldl')

-- | Implements @xor@ operation on list of boolean values
xor :: [Bool] -- ^ list of Bool values
    -> Bool   -- ^ list folded with @xor@
xor = foldl' (\ x y -> not (x && y) && (x || y)) False


-- | Analog of @map@ implemented with `fodlr`
map' :: (a -> b) -- ^ mapping function
     -> [a]      -- ^ list of values to map
     -> [b]      -- ^ mapped values list
map' f = foldr (\ x xs -> f x : xs) []


-- | Analog of @foldl@ implemented with `foldr`
myFoldl :: (a -> b -> a) -- ^ function to fold two values
        -> a             -- ^ initial value
        -> [b]           -- ^ list to fold
        -> a             -- ^ list folded with specified function
myFoldl f = foldr (flip f)


-- | Calculates list of primes in scope from 3 to @2n + 2@
sieveSundaram :: Integer   -- ^ n
              -> [Integer] -- ^ list of primes from 3 to @2n + 2@
sieveSundaram n = map (\m -> 2 * m + 1) . filter (`notElem` ms) $ ij_range
    where
        ij_range = [1..n]
        ijs      = cartProd ij_range ij_range
        ms       = map (\ (i,j) -> 2 * i * j + i + j ) ijs



cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
