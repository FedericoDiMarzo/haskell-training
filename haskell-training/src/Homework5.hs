{-# LANGUAGE FlexibleInstances #-}

module Homework5 where

import ExprT
import Parser
import StackVM

-- exercise 1
eval :: ExprT -> Integer
eval (Lit n) = n
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b

-- exercise 2
evalStr :: String -> Maybe Integer
evalStr s =
  let parsed = parseExp Lit ExprT.Add ExprT.Mul s
   in case parsed of
        Nothing -> Nothing
        Just exp -> Just (eval exp)

-- exercise 3
class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  mul :: e -> e -> e

instance Expr ExprT where
  lit n = Lit n
  add a b = ExprT.Add a b
  mul a b = ExprT.Mul a b

-- exercise 4
instance Expr Integer where
  lit x = x
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit x
    | x <= 0 = False
    | otherwise = True
  add a b = a || b
  mul a b = a && b

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add a@(MinMax x) b@(MinMax y) = MinMax (max x y)
  mul a@(MinMax x) b@(MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit x = Mod7 x
  add a@(Mod7 x) b@(Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul a@(Mod7 x) b@(Mod7 y) = Mod7 ((x * y) `mod` 7)

-- texts exercise 4
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer

testBool = testExp :: Maybe Bool

testMM = testExp :: Maybe MinMax

testSat = testExp :: Maybe Mod7

-- exercise 5
instance Expr Program where
  lit x = [PushI x]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile s = parseExp lit add mul s :: Maybe Program