{-# OPTIONS_GHC -Wall #-}

{-|
Module      : JoinList
Description : CIS194 homework #7
Copyright   : (c) Sergey Vernidub 2018
License     : GPL-3
Maintainer  : svernidub@cropio.com
Stability   : experimental
Portability : POSIX

Contains data structure JoinList declaration
-}

module JoinList (
    JoinList(..),
    (+++),
    indexJ,
    dropJ,
    takeJ,
    jlToList,
    (!!?),
) where


import           Sized

-- | Structure to store data with attributes
data JoinList m a = Empty      -- ^ empty tree element
                  | Single m a -- ^ element containing data
                  | Append m (JoinList m a) (JoinList m a) -- ^ node of tree
                  deriving (Eq, Show)


-- | Instance of @Sized@ for @JoinLost@ allows measure it size
instance Sized m => Sized (JoinList m a) where
    size Empty          = 0
    size (Single m _)   = size m
    size (Append m _ _) = size m


-- | Combines two trees in one
(+++) :: Monoid m
      => JoinList m a -- ^ Left branch of future tree
      -> JoinList m a -- ^ Right branch of future tree
      -> JoinList m a -- ^ Resulting tree
l +++ r = Append (tag l `mappend` tag r) l r


-- | Calculates returning tag of @Single m a@ or
--   appending tags of left and right branches of node
tag :: Monoid m
    => JoinList m a -- ^ node
    -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append _ l r) = tag l `mappend` tag r


-- | Returns element of tree with index in equal list
indexJ :: (Sized b, Monoid b)
       => Int          -- ^ index
       -> JoinList b a -- ^ tree
       -> Maybe a      -- ^ element
indexJ i = findByNumber (i + 1)


-- | Drops specified number of elements in tree
dropJ :: (Sized b, Monoid b)
      => Int          -- ^ number of elements to drop from tree
      -> JoinList b a -- ^ tree
      -> JoinList b a -- ^ tree without elements
dropJ _ Empty = Empty

dropJ n e@(Single _ _)
    | n > 0 = Empty
    | otherwise = e

dropJ n (Append m l r)
    | Size n > size m = Empty
    | Size n < size l = dropJ n l +++ r
    | Size n == size l = r
    | Size n > size l = let (Size leftSize) = size l
                            k               = n - leftSize
                        in dropJ k r

dropJ _ _ = error "Could not drop nodes!"


-- | Returns subtree that contains specified number of elements
takeJ :: (Sized b, Monoid b)
      => Int          -- ^ number of elements in subtree
      -> JoinList b a -- ^ tree
      -> JoinList b a -- ^ subtree

takeJ _ Empty = Empty

takeJ n e@(Single _ _)
    | n == 0 = Empty
    | otherwise = e

takeJ n e@(Append m l r)
    | Size n >= size m = e
    | Size n == size l = l
    | Size n <  size l = takeJ n l
    | Size n >  size l = let (Size lSize) = size l
                         in l +++ takeJ (n - lSize) r

takeJ _ _ = error "Could not take nodes!"


-- | Converts tree to list
jlToList :: JoinList m a -- ^ input tree
         ->  [a]         -- ^ resulting list
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


-- | Safe element search in list
(!!?) :: [a]     -- ^ list
      -> Int     -- ^ index of element
      -> Maybe a -- ^ element
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)


-- | Finds element by its number in tree (number = index + 1)
findByNumber :: (Sized b, Monoid b)
             => Int          -- ^ number
             -> JoinList b a -- ^ tree
             -> Maybe a      -- ^ element
findByNumber _ Empty   = Nothing

findByNumber n (Single _ a)
    | n == 1           = Just a
    | otherwise        = Nothing

findByNumber n (Append m l r)
    | Size n >  size m = Nothing
    | Size n <= size l = findByNumber n l
    | Size n >  size l = let (Size sl ) = size l
                         in findByNumber (n - sl) r

findByNumber _ _ = error "Could not find node!"
