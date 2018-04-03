{-# OPTIONS_GHC -Wall #-}

module Homework where

import Data.List ((\\))

-- Exercise 1
-- Implement the following functions using wholemeal programming practices.
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- Wholemean implementations
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate collatz
  where
    collatz n
      | even n    = n `div` 2
      | otherwise = 3 * n + 1


-- Exercise 2
-- Build a foldTree that builds a balancend binary tree using foldr
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Insert the given item in the given tree, keeping the tree balanced
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node _ left y right) =
  let
    heightOf tree = case tree of
      Leaf -> 0
      (Node h _ _ _) -> h
    hright = heightOf right
    hleft = heightOf left
  in if hleft <= hright then
      Node (hright + 1) (insert x left) y right
    else
      Node (hleft + 1) left y (insert x right)

-- Builds a binary balanced Tree with the given list of items.
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf


-- Exercise 3.1
-- Returns True if and only if there are an odd
-- number of True values contained in the input list
-- Alternative implementation without fold: xor = odd . length . filter id
xor :: [Bool] -> Bool
xor = foldr (/=) False


-- Exercise 3.2
-- Implement map as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- Exercise 3.3
-- Implement foldl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- Exercise 4
-- Implement the sieve of Sundaram for finding all the primes
-- between 2 and n where 2 <= n.
marked :: Integer -> [Integer]
marked n = [1 .. n] \\ list
  where
    cartesian = [ (x, y) | x <- [1 .. n]
                         , y <- [1 .. n] ]
    ijform a b = a + b + 2 * a * b
    list = (
      map (uncurry ijform) . filter (uncurry (<=))
      ) cartesian

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : (filter (<= n) . map (\k -> 2 * k + 1) . marked) n

-- Utility functions to test the algorithm
-- True if given number is prime, False otherwise
isPrime :: Integer -> Bool
isPrime k = null [ x | x <- [2 .. isqrt k], k `mod` x == 0 ]
  where
    isqrt n = floor . sqrt $ (fromIntegral n :: Float)

-- Computes primes up to the given number and check primality of all elements
checkSieveSundaramOf :: Integer -> Bool
checkSieveSundaramOf = all isPrime . sieveSundaram
