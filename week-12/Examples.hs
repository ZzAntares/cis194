{-# OPTIONS_GHC -Wall #-}

module Examples where


class MyMonad m where
  return' :: a -> m a

  (.>>=) :: m a -> (a -> m b) -> m b

  (.>>) :: m a -> m b -> m b
  m1 .>> m2 = m1 .>>= const m2


instance MyMonad Maybe where
  return' = Just
  Nothing .>>= _ = Nothing
  Just x .>>= f = f x


check :: Int -> Maybe Int
check n | n < 10    = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n    = Just $ n `div` 2
        | otherwise = Nothing


ex01 = return' 7 .>>= check .>>= halve
ex02 = return' 12 .>>= check .>>= halve
ex03 = return' 12 .>>= halve .>>= check


instance MyMonad [] where
  return' x = [x]
  xs .>>= f = concat $ map f xs


addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x + 1, x + 2]

ex04 = [10, 20, 30] .>>= addOneOrTwo


sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (mx:mxs) = mx .>>= (\x -> sequence' mxs .>>= \xs -> return (x:xs))

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m = sequence' $ replicate n m
