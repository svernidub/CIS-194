{-# OPTIONS_GHC -Wall #-}

{-|
Module      : Wholemeal
Description : CIS194 homework #4
Copyright   : (c) Sergey Vernidub 2018
License     : GPL-3
Maintainer  : svernidub@cropio.com
Stability   : experimental
Portability : POSIX

Module with functions rewrited with wholemeal practices
-}

module Wholemeal (
    fun1,
    fun1',
    fun2,
    fun2'
) where

import           Data.List (foldl', iterate, takeWhile)

-- | Original function @fun1@
fun1 :: [Integer] -- ^ List of integers
     -> Integer   -- ^ Computed integer
fun1 []                 = 1
fun1 (x:xs) | even x    = (x - 2) * fun1 xs
            | otherwise = fun1 xs

-- | @fun1'@ is analog of @fun1@ rewrited with wholemeal practicies
fun1' :: [Integer] -- ^ List of integers
      -> Integer   -- ^ Computed integer
fun1' = foldl' (*) 1 . map (subtract 2) . filter even


-- | Original function @fun2@
fun2 :: Integer -- ^ List of integers
     -> Integer -- ^ Computed integer
fun2 1             = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)


-- | @fun2'@ is analog of @fun2@ rewrited with wholemeal practicies
fun2' :: Integer -- ^ List of integers
      -> Integer -- ^ Computed integer
fun2' = foldl' (+) 0
        . filter even
        . takeWhile (/= 1)
        . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)
