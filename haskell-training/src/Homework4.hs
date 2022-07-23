module Homework4 where

import Data.List

-- exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldr (\x y -> (x - 2) * y) 1 (filter even xs)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n = foldr (+) 0 (filter even (takeWhile (/=1) (iterate f n)))
  where 
    f x = f_even x + f_odd x
    f_even x = (1 - (x `mod` 2)) * (x `div` 2)
    f_odd x = (x `mod` 2) * (3 * x + 1)
