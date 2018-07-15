module Luhn (
    validate
) where


toDigits :: Integer -> [Integer]
toDigits n | n <= 0    = []
           | otherwise = toDigits numHead ++ [digit]
                 where
                     digit   = mod n 10
                     numHead = div n 10


toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse
                      where
                          doubleEveryOther' []         = []
                          doubleEveryOther' [x]        = [x]
                          doubleEveryOther' (x1:x2:xs) = x1 : x2 * 2 : doubleEveryOther' xs


sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits [x]    = x
sumDigits (x:xs) = sumNumberDigits x + sumDigits xs
                       where
                           sumNumberDigits = sum . toDigits


validate :: Integer -> Bool
validate ccn = sum ccn `mod` 10 == 0
                   where
                      sum = sumDigits . doubleEveryOther . toDigits
