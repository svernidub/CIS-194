{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party (
    glCons,
    moreFun,
    treeFold,
    nextLevel,
    maxFun,
    main
) where

import           Data.Tree
import           Employee  (Employee (..), GuestList (..))

-- | Instance of @class@
instance Monoid GuestList where
    mempty                        = GL [] 0
    mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)


-- | Adds @Employee@ to @GuestList@
glCons :: Employee  -- ^ employee to insert
       -> GuestList -- ^ @GuestList@ where insert
       -> GuestList -- ^ resulting @GuestList@
glCons employee (GL list fun) = GL newList newFun
    where
        newList = list ++ [employee]
        newFun = fun + empFun employee


-- | Takes two @GuestList@s and returns list with max fun
moreFun :: GuestList -- ^ first list
        -> GuestList -- ^ second list
        -> GuestList -- ^ list with more fun
moreFun l1 l2 = case compare l1 l2 of
                    GT -> l1
                    _  -> l2


-- | Fold for @Tree@
treeFold :: (a -> [b] -> b) -- ^ function to fold with
         -> b               -- ^ initial value
         -> Tree a          -- ^ tree to fold
         -> b               -- ^ tree folded to value
treeFold f i (Node x xs) = f x (map (treeFold f i) xs)


-- | Takes boss of subtree and list of results for each subtree under boss.
--   Result contains pair of @GuestList@s:
--   (1) best possible @GuestList@ with the boss
--   (2) best possible @GuestList@ without the boss
nextLevel :: Employee                 -- ^ Boss of subtree
          -> [(GuestList, GuestList)] -- ^ List of results for each subtree
          -> (GuestList, GuestList)   -- ^ Best result
nextLevel boss ls = (maxWithBoss, maxWithoutBoss)
    where
        withSubBoss       = map fst ls
        withoutSubBoss    = map snd ls
        maxWithBoss       = glCons boss . maximum $ withSubBoss ++ [mempty]
        maxWithoutBoss    = maximum (withoutSubBoss ++ [mempty])


-- | Calculates guest list with tree of employees
maxFun :: Tree Employee
       -> GuestList
maxFun tree = max withBoss withoutBoss
    where
        (withBoss, withoutBoss) = treeFold nextLevel (mempty, mempty) tree


parseTreeAndPrintMaxList :: String -> IO ()
parseTreeAndPrintMaxList = printTree . maxFun . read


printTree :: GuestList -> IO ()
printTree (GL employees totalFun) = do
        putStrLn $ "Total fun: " ++ show totalFun
        mapM_ (putStrLn . empName) employees
        return ()


main :: IO ()
main = readFile "company.txt" >>= parseTreeAndPrintMaxList

