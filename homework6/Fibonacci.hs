{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-|
Module      : Fibonacci
Description : CIS194 homework #6
Copyright   : (c) Sergey Vernidub 2018
License     : GPL-3
Maintainer  : svernidub@cropio.com
Stability   : experimental
Portability : POSIX

Fibonacci lazy calculations
-}

module Fibonacci (
    Stream(..),
    fib,
    fibs1,
    fibs2,
    streamRepeat,
    streamToList,
    streamMap,
    streamFromSeed,
    nats,
    ruler,
    interleaveStreams,
    fx,
    fib3,
    fib4
) where


-- | Stream type
data Stream a = Cons a (Stream a) -- ^ Constructor of stream


-- | Matrix type
data Matrix = Matrix Integer Integer Integer Integer


-- | Instance of @Show@ for @Stream a@
instance Show a => Show (Stream a) where
    show = foldr ((++) . (++ ", ") . show) "..." . take 5 . streamToList


-- | Instance of @Num@ for @Stream Integer@
instance Num (Stream Integer) where
    fromInteger n                  = Cons n (streamRepeat 0)
    negate                         = streamMap negate
    (Cons x xs)  +    (Cons y ys)  = Cons (x + y) (xs + ys)
    (Cons x xs') * ys@(Cons y ys') = Cons (x * y) (streamMap (*x) ys' + xs' * ys )


-- | Instance of @Num@ for @Stream Integer@
instance Fractional (Stream Integer) where
    (Cons x xs') / (Cons y ys') =
        let q = Cons (div x y) (streamMap (`div` y) (xs' - q * ys'))
        in q


-- | Instance of @Num@ for @Matrix@
instance Num Matrix where
    (Matrix x00 x01 x10 x11) * (Matrix y00 y01 y10 y11) = Matrix z00 z01 z10 z11
        where
            z00 = x00 * y00 + x01 * y10
            z01 = x00 * y01 + x01 * y11
            z10 = x10 * y00 + x11 * y10
            z11 = x10 * y01 + x11 * y11


-- | Recursive implementation of Fibonacci numbers (slow)
fib :: Integer -- ^ number of member
    -> Integer -- ^ member
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib(n - 2)


-- | Recursive implementation of infinite list of Fibonacci
--   numers (slow)
fibs1 :: [Integer] -- ^ list of Fibonacci numers
fibs1 = fibs 0
    where
        fibs :: Integer -> [Integer]
        fibs n = fib n : fibs(n + 1)


-- | Recursive implementation of Fibonacci numbers (fast)
fibs2 :: [Integer] -- ^ list of Fibonacci numers
fibs2 = fibs2' 0 1
    where
        fibs2' :: Integer -> Integer -> [Integer]
        fibs2' n1 n2 = let n3 = n1 + n2
                       in  n1 : fibs2' n2 n3


-- | Converts @Stream a@ to list
streamToList :: Stream a -- ^ stream
             -> [a]      -- ^ list
streamToList (Cons a s) = a : streamToList s


-- | creates repeating specified value stream
streamRepeat :: a        -- ^ value that stream should repeat
             -> Stream a -- ^ stream with copies of value
streamRepeat x = Cons x (streamRepeat x)


-- | Applies a function to every element of stream
streamMap :: (a -> b) -- ^ function to map values
          -> Stream a -- ^ input stream
          -> Stream b -- ^ output stream
streamMap f (Cons x s) = Cons (f x) (streamMap f s)




-- | Generates a @Stream a @ from a seed of type @a@
streamFromSeed :: (a -> a) -- ^ function to transform the seed
               -> a        -- ^ seed
               -> Stream a -- ^ output Stream
streamFromSeed f x = Cons x (streamFromSeed f (f x))


-- | Generate stream of natural numbers
nats :: Stream Integer -- ^ output strem
nats = streamFromSeed (+1) 0


-- | Alternates the elements from two streams
interleaveStreams :: Stream a -- ^ first stream
                  -> Stream a -- ^ second stream
                  -> Stream a -- ^ result stream
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)


-- | Ruler function
ruler :: Stream Integer
ruler = foldr (interleaveStreams . streamRepeat) (streamRepeat 0) [1..]


-- | Base list for calculating Fibonacci numbers
fx :: Stream Integer
fx = Cons 0 $ Cons 1 $ streamRepeat 0


-- | Calculate list of Fibonacci numbers
fib3 :: Stream Integer
fib3 = fx / (1 - fx - fx^2)


-- | Fast @nth@ Fibonacci number calc using raising matrix to power n
fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = fn
    where
        matrix = Matrix 1 1 1 0
        (Matrix fn _ _ _) = matrix ^ (n - 1)
