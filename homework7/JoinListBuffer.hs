{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}


{-|
Module      : JoinListBuffer
Description : CIS194 homework #7
Copyright   : (c) Sergey Vernidub 2018
License     : GPL-3
Maintainer  : svernidub@cropio.com
Stability   : experimental
Portability : POSIX

Defines @Buffer@ instance for @JoinList@ that allows
using of @JoinList@ as backand for @Editor@
-}


module JoinListBuffer (
    Buffer(..),
    JoinList(..)
) where

import           Buffer
import           JoinList
import           Scrabble
import           Sized


-- | Instance for class @Buffer@ for @JoinList (Score, Size)@
instance Buffer (JoinList (Score, Size) String) where
    toString             = unlines . jlToList
    fromString           = composeJl
    line                 = indexJ
    replaceLine n        = replaceLineInNode (n + 1)
    numLines jl          = let (Size s) = size jl in s

    value Empty                     = 0
    value (Single (Score s, _) _)   = s
    value (Append (Score s, _) _ _) = s


replaceLineInNode :: Int
                  -> String
                  -> JoinList (Score, Size) String
                  -> JoinList (Score, Size) String
replaceLineInNode 1 str Empty        = fromString str
replaceLineInNode 1 str (Single _ _) = fromString str

replaceLineInNode n str node@(Append (_, Size s) left right)
    | n > s = node
    | Size n <= size left = replaceLineInNode n str left +++ right
    | otherwise = left +++ replaceLineInNode newN str right
                      where
                        (Size lSize) = size left
                        newN         = n - lSize

replaceLineInNode n str node = error $ "Error: n=" ++ show n ++
                                       " str=" ++ str ++
                                       " node=" ++ show node


composeJl :: String
          -> JoinList (Score, Size) String

composeJl = foldl1 (+++) . map (\s -> Single (scoreString s, 1) s) . lines
