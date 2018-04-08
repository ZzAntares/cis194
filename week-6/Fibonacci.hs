{-# OPTIONS_GHC -Wall -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

import Data.List as List

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [ fib n | n <- [0..] ]


-- Exercise 2
fibs2 :: [Integer]
fibs2 = map fst $ iterate (\ (a, b) -> (b, a + b)) (0, 1)
-- fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)  -- Cleaner implementation


-- Exercise 3
-- Data type of polymorphic streams
data Stream a = Cons a (Stream a)

-- Convert a Stream to an infinite list.
streamToList :: Stream a -> [a]
streamToList (Cons el stream) = el : streamToList stream

-- Instance of Show for Stream, only shows a prefix of the first 20 elements
instance Show a => Show (Stream a) where
  show stream = show $ take 20 $ streamToList stream


-- Exercise 4
-- Generates a stream containing infinite copies of the given input
streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

-- Applies a function to every element of a Stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons el stream) = Cons (f el) $ streamMap f stream

-- Generates a Stream from a seed "a"
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed g a = let b = g a in Cons a $ streamFromSeed g b


-- Exercise 5
-- Contains the infinite list of natural numbers
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = streamMap maxPow2Div nats where
  maxPow2Div n = case List.find (\ a -> mod n (2^a) == 0) [n, (n - 1) .. 1] of
    Nothing -> 0
    Just value -> fromIntegral value


-- Exercise 6
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate (Cons n stream) = Cons (negate n) (negate stream)
  (+) (Cons a astream) (Cons b bstream) = Cons (a + b) (astream + bstream)
  (*) (Cons a atail) bstream@(Cons b btail) = Cons (a * b) (xaob + ab) where
    xaob = streamMap (* a) btail
    ab = atail * bstream

instance Fractional (Stream Integer) where
  (/) (Cons a atail) (Cons b btail) = q where
    q = Cons (div a b) $ streamMap (`div` b) (atail - q * btail)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)


-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer deriving Show

instance Num Matrix where
  (*) (Matrix a b c d) (Matrix w xx y z) =
    Matrix (a*w + b*y) (a*xx + b*z) (c*w + d*y) (c*xx + d*z)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = fibo (f ^ n) where
  f = Matrix 1 1 1 0
  fibo (Matrix _ a _ _) = a
