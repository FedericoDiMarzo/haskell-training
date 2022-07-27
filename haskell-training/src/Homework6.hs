module Homework6 where

import Data.Bits

-- exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n -1) + fib (n -2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- exercise 2
fibs2 :: [Integer]
fibs2 = [x | (x, _) <- iterate (\(x, y) -> (y, x + y)) (0, 1)]

-- exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList s@(Cons x s') = x : streamToList s'

instance Show a => Show (Stream a) where
  show s =
    let xs = streamToList s
     in show $ take 40 xs

-- exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f s@(Cons x s') = Cons (f x) (streamMap f s')

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed seed s = Cons s (streamFromSeed seed (seed s))

-- exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

streamsInterleave :: Stream a -> Stream a -> Stream a
streamsInterleave a@(Cons x a') b@(Cons y b') = Cons x (Cons y (streamsInterleave a' b'))

gcdPow2 :: Integer -> Integer
gcdPow2 n = fromIntegral $ length shifted_list
  where
    shifted_list = takeWhile even (iterate (\x -> shift x (-1)) n)
    

ruler :: Stream Integer
ruler = streamMap gcdPow2 nats'
  where 
    nats' = streamMap (+1) nats