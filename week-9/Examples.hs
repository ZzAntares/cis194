{-# OPTIONS_GHC -Wall #-}

module Examples where

class MyFunctor f where
  myfmap :: (a -> b) -> f a -> f b


instance MyFunctor Maybe where
  myfmap _ Nothing = Nothing
  myfmap f (Just x) = Just (f x)

instance MyFunctor [] where
  myfmap _ [] = []
  myfmap f (x : xs) = f x : myfmap f xs

instance MyFunctor (Either a) where
  myfmap f (Right x) = Right (f x)
  myfmap _ (Left x) = Left x

instance MyFunctor IO where
  myfmap f ioa = ioa >>= (return . f)

instance MyFunctor ((->) e) where
  myfmap f oe = f . oe
  -- or: myfmap = (.)
