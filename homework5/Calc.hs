{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

{-|
Module      : Calc
Description : CIS194 homework #5
Copyright   : (c) Sergey Vernidub 2018
License     : GPL-3
Maintainer  : svernidub@cropio.com
Stability   : experimental
Portability : POSIX

Module that implements some expressions processors
-}

module Calc (
    Expr(..),
    HasVars(..),
    MinMax(..),
    Mod7(..),
    eval,
    evalStr,
    reify,
    compile,
    withVars
) where

import qualified Data.Map as M
import           ExprT
import           Parser
import qualified StackVM  as S


-- | Class @Expr@ describes types for expressions
class Expr a where
    lit :: Integer -> a -- ^ literal for expression
    add :: a -> a -> a  -- ^ sum operation
    mul :: a -> a -> a  -- ^ product operation


-- | Class @HasVars@ describes types for expressions that
--   can contains variables
class HasVars a where
    var :: String -> a  -- ^ variable name


-- | Type for expressions that support variables, literals, and two
--   operations - sum and multiplication
data VarExprT = Literal  Integer           -- ^ Literal in @VarExpT@ expression
              | Variable String            -- ^ Variable in @VarExpT@ expression
              | Sum VarExprT VarExprT      -- ^ Sum operation in expression
              | Multiply VarExprT VarExprT -- ^ Multiply operation in expression
              deriving (Show, Eq)


-- | Type for expressions that support two operations - min and max
newtype MinMax = MinMax Integer  -- ^ Literal in @MinMax@ expression
                deriving (Eq, Show)



-- | Type for expressions with numbers in range 0..6
newtype Mod7 = Mod7 Integer -- ^ Literal in @Mod7@ expression
               deriving (Eq, Show)


-- | Implementation of class @Expr@ for @ExprT@
instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- | Implementation of class @Expr@ for @Bool@
instance Expr Bool where
    lit = (<) 0
    add = (||)
    mul = (&&)

-- | Implementation of class @Expr@ for @Integer@
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

-- | Implementation of class @Expr@ for @MinMax@
instance Expr MinMax where
    lit                       = MinMax
    add (MinMax l) (MinMax r) = MinMax $ min l r
    mul (MinMax l) (MinMax r) = MinMax $ max l r


-- | Implementation of class @Expr@ for @Mod7@
instance Expr Mod7 where
    lit n = Mod7 $ n `mod` 7
    add (Mod7 l) (Mod7 r) = lit $ l + r
    mul (Mod7 l) (Mod7 r) = lit $ l * r

-- | Implementation of class @Expr@ for @StackVM.Program@
instance Expr S.Program where
    lit n = [S.PushI n]
    add l r = l ++ r ++ [S.Add]
    mul l r = l ++ r ++ [S.Mul]


-- | Implementation of class @Expr@ for @VarExprT@
instance Expr VarExprT where
    lit = Literal
    add = Sum
    mul = Multiply


-- | Implementation of class @HasVars@ for @VarExprT@
instance HasVars VarExprT where
    var = Variable


-- | Implementation of class @HasVars@ for variable mapping
instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup


-- | Implementation of class @Expr@ for variable mapping
instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n _  = Just n
    add l r vars = applicateOperation l r vars (+)
    mul l r vars = applicateOperation l r vars (*)


-- | Identifies polimorphic expressions tree to ExprT
reify :: ExprT -> ExprT
reify = id


-- | Runs ExptrT expression
eval :: ExprT -> Integer
eval (Lit i)   = i
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r


-- | Parses and evaluates expression
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
                Just expr -> Just $ eval expr
                Nothing   -> Nothing



-- | Compiles expression to StackVM program
compile :: String -> Maybe S.Program
compile s = case parseExp lit add mul s of
                Just program -> Just program
                Nothing      -> Nothing


-- | Evaluates VarExprT expression with context specified as variables map
withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs expr = expr $ M.fromList vs


applicateOperation :: (M.Map String Integer -> Maybe Integer)
                   -> (M.Map String Integer -> Maybe Integer)
                   -> M.Map String Integer
                   -> (Integer -> Integer -> Integer)
                   -> Maybe Integer
applicateOperation l r vars f = case (l vars, r vars) of
                                    (Just ln, Just rn) -> Just $ f ln rn
                                    _                  -> Nothing
