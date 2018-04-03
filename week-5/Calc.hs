{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser
import qualified StackVM as SVM
import qualified Data.Map as Map


-- Exercise 1: Write an evaluator for ExprT expressions
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y


-- Exercise 2: Evaluate String expressions
evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
  Just exprt -> Just (eval exprt)
  _ -> Nothing


-- Excercise 3: Create the Expr type class
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a


instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- Checker
ex3 :: Bool
ex3 = (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) == Mul (Add (Lit 2) (Lit 3)) (Lit 4)


-- Exercise 4: Provide instances of Expr for Integer, Bool, MinMax and Mod7 data types
instance Expr Integer where
  lit x = x
  add = (+)
  mul = (*)


instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

-- For defining other instances relaying on Integer we need to create a newtype
newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)
instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit x = Mod7 $ mod x 7
  add (Mod7 x) (Mod7 y) = lit (x + y)
  mul (Mod7 x) (Mod7 y) = lit (x * y)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7


-- Exercise 5: Make a compiler for arithmetic expressions.
instance Expr SVM.Program where
  lit x = [SVM.PushI x]
  add x y = x ++ y ++ [SVM.Add]
  mul x y = x ++ y ++ [SVM.Mul]

compile :: String -> Maybe SVM.Program
compile = parseExp lit add mul

-- Exercise 6: Add support for variables
class HasVars a where
  var :: String -> a

data VarExprT = VarLit Integer
              | VarAdd VarExprT VarExprT
              | VarMul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VarLit
  add = VarAdd
  mul = VarMul

instance HasVars VarExprT where
  var = Var

-- For mapping variables to values
-- instance Expr (Map.Map String Integer -> Maybe Integer) where
--   lit = const . Just
--   -- Couldn't implement add and mul =S

instance HasVars (Map.Map String Integer -> Maybe Integer) where
  var = Map.lookup
