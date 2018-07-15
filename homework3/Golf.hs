{-|
Module      : Golf
Description : CIS 194 Homework #3
Copyright   : (c) Sergey Vernidub, 2018
License     : GPL-3
Maintainer  : svernidub@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Golf (
    skips,
    localMaxima,
    histogram
) where


-- | @skips xs@ returns list contains xs, copy of xs without first element,
--   second element etc.
--
-- >>> skips "ABCD"
-- ["ABCD", "BD", "C", "D"]
--
-- >>> skips "hello!"
-- ["hello!", "el!", "l!", "l", "o", "!"]
--
-- >>> skips [1]
-- [[1]]
--
-- >>> skips [True, False]
-- [[True, False], [False]]
--
-- >>> skips []
-- []
skips :: [a]   -- ^ input list
      -> [[a]] -- ^ returns list of of lists
skips [] = []
skips xs = selectEach 1 xs
    where
        selectEach n ys | n > length ys = []
                        | otherwise     = let ewis = zip ys [1..]
                                              f x      = snd x `mod` n == 0
                                              fewis = filter f ewis
                                              es = map fst fewis
                                          in es : selectEach (n+1) ys


-- | @localMaxima xs@ looks for local maximums in list
--
--  for @[]@ it returns @[]@ because empty list can't cointain maximums
--  for @[x]@ it returns @[]@ because single element can't have neighboring elements
--
--  for list of elements:
--  * it skips first element because it can't have neighboring elements
--    but use it as currently found maximum
--  * searching local maximas comparing last founded maximal element with
--    next element:
--    if it is gt/eq next element it is local maxima
--     otherwise it isn't maxima and we recursively check next element
--
-- >>> localMaxima [2,9,5,6,1]
-- [9,6]
--
-- >>> localMaxima [2,3,4,1,5]
-- [4]
--
-- >>> localMaxima [1,2,3,4,5]
-- []
localMaxima :: [Integer] -- ^ list of Integers
            -> [Integer] -- ^ list of local maximums
localMaxima []     = []
localMaxima [x]    = []
localMaxima (x0:x1:xs) = localMaxima' x1 xs
    where
        localMaxima' :: Integer -> [Integer] -> [Integer]
        localMaxima' _  []         = []
        localMaxima' cm (x:xs) | cm <= x = localMaxima' x xs
                               | cm >  x = cm : localMaxima' x xs

-- | @histogram xs@ accepts as argument list of numbers
--   and returns ASCII histogram of its frequences
--
--
-- >>> histogram [3,5]
-- "   * *    \n==========\n0123456789\n"

histogram :: [Integer] -- ^ list of numbers
          -> String    -- ^ historgram of frequencies
histogram xs = concat $ reverse allRows
    where
        header   = concatMap show histRange ++ "\n"
        fs       = frequences xs
        dataRows = buildDataRows fs
        hrow     = replicate (length histRange) '=' ++ "\n"
        allRows  = header : hrow : dataRows


-- | @frequences xs@ accepts list of integers and returns list
--   where each element represents frequence of its index in @xs@
--
-- >>> frequences [1,1,1,2,2,2,3,3,4]
-- [0, 3, 3, 2, 1, 0, 0, 0, 0, 0]
--
-- >>> frequences []
-- [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
frequences :: [Integer] -- ^ list of numbers
           -> [Integer] -- ^ returns list of frequesnces
frequences xs = map (\i -> iLength (findN i xs)) histRange
    where
        findN i = filter (== i)
        iLength = fromIntegral . length

-- | @buildDataRows fs@ accepts list of frequesnces and return list of rows
--   for ASCII histogram with frequesnces levels
--
-- >>> buildDataRows [0,1,1,1,4,1,2,0,0,1]
-- [" ******  *\n","    * *   \n","    *     \n","    *     \n"]
buildDataRows :: [Integer] -- ^ list of frequences
              -> [String]  -- ^ list of histogram rows
buildDataRows fs = buildRow 1
    where
        max                   = maximum fs
        buildRow n | n <= max = dataCells n fs : buildRow (n + 1)
                   | otherwise = []

-- | @dataCells n fs@ accepts level of frequence and list of frequesnces
--
-- >>> dataCells 2 [0,1,1,1,4,1,2,0,0,1]
-- "    * *   \n"
dataCells :: Integer   -- ^ level of frequesncy
          -> [Integer] -- ^ list of frequesnces
          -> String    -- ^ row for current level of frequesnces
dataCells _ []     = "\n"
dataCells n (f:fs) = cell
    where
        placeholder = " "
        cell        | f >= n = "*" ++ dataCells n fs
                    | otherwise  = " " ++ dataCells n fs

-- | @histRange@ numers for frequence histogram
histRange = [0..9]
