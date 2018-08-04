{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

{-|
Module      : Scrabble
Description : CIS194 homework #7
Copyright   : (c) Sergey Vernidub 2018
License     : GPL-3
Maintainer  : svernidub@cropio.com
Stability   : experimental
Portability : POSIX

Calculationg of score for string using the rules of game Scrabble
-}


module Scrabble (
    Score(..),
    score,
    scoreString,
    scoreLine
) where


import           JoinList


-- | Type for score
newtype Score = Score Int deriving (Show, Num)


-- | Instace for class @Monoid@ for @Score@
instance Monoid Score where
    mempty  = Score 0
    mappend = (+)


-- | Mapps @Char@ to @Score@ using the rules of Scrabble
score :: Char  -- ^ character
      -> Score -- ^ score for character
score 'a' = Score 1
score 'b' = Score 3
score 'c' = Score 3
score 'd' = Score 2
score 'e' = Score 1
score 'f' = Score 4
score 'g' = Score 2
score 'h' = Score 4
score 'i' = Score 1
score 'j' = Score 8
score 'k' = Score 5
score 'l' = Score 1
score 'm' = Score 3
score 'n' = Score 1
score 'o' = Score 1
score 'p' = Score 3
score 'q' = Score 10
score 'r' = Score 1
score 's' = Score 1
score 't' = Score 1
score 'u' = Score 1
score 'v' = Score 4
score 'w' = Score 4
score 'x' = Score 8
score 'y' = Score 4
score 'z' = Score 10
score  _  = Score 0


-- | Calculates @Score@ for @String@ using the rules of Scrabble
--   as sum of characters
scoreString :: String -- ^ input string
            -> Score  -- ^ calculated score
scoreString = foldl mappend 0 . map score


-- | Takes string calculates its score than pack it to @JoinList@ node
scoreLine :: String                -- ^ input string
          -> JoinList Score String -- ^ output @JoinList@
scoreLine str = Single (scoreString str) str
