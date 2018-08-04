{-# OPTIONS_GHC -Wall #-}

module Main where

import           Editor
import           JoinList
import           JoinListBuffer ()
import           Scrabble
import           Sized

-- | initial buffer state
initBuf :: JoinList (Score, Size) String
initBuf = Append (Score 23, Size 2)
            (Single (Score 9, Size 1) "yay ")
            (Single (Score 14, Size 1) "haskell!")


-- | main
main :: IO()
main = runEditor editor initBuf

