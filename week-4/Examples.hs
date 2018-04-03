module Examplese where

greaterThan100 :: [Integer] -> [Integer]
greaterThan100 = filter (> 100)

sampleFunction :: (b -> c) -> (a -> b) -> (a -> c)
sampleFunction f g = f . g


-- Focusing in a single element at a time is not haskellish
foobar :: [Integer] -> Integer
foobar [] = 0
foobar (x:xs)
  | x > 3 = (7 * x + 2) + foobar xs
  | otherwise = foobar xs

foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7 * x + 2) . filter (> 3)

-- Combine all elements into one
fold :: b -> (a -> b -> b) -> [a] -> b
fold z _ [] = z
fold z f (x:xs) = f x (fold z f xs)

product' :: [Integer] -> Integer
product' = fold 1 (*)

sum' :: [Integer] -> Integer
sum' = fold 0 (+)

length' :: [Integer] -> Integer
length' = fold 0 (const (+1))

-- foobar with fold
foobar'' :: [Integer] -> Integer
foobar'' = fold 0 (\el acc -> acc + (el * 7 + 2)) . filter (> 3)
