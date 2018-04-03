{-# OPTIONS_GHC -Wall #-}

module Examples where

-- Parametric polymorphism functions promise to handle any data type
f1 :: a -> a -> a
f1 x _ = x

f2 :: a -> a -> a
f2 _ y = y

-- This is a type class polymorphic function declaration, a restricted promise
-- that the function will work for any type the caller chooses, as long as the
-- chosen type is an instance of the required type class(es).
-- (==) :: Eq a => a -> a -> a

-- Declare our own data type and add it as an instance of the Eq type class.
data Foo = F Int
         | G Char

instance Eq Foo where
  -- Define how it should behave on (==) and (/=)
  (F a) == (F b) = a == b
  (G a) == (G b) = a == b
  _ == _         = False

  one /= two     = not (one == two)

-- When declaring a type class we can add default implementations of methods
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   -- This is a default implementation for (/=), but
--   -- can be overriden by instances of the type class.
--   x /= y = not (x == y)

-- This tells GHC to automatically derive instances of
-- the Eq, Ord, and Show type classes for our data type Foo.
data Foo' = F' Int
          | G' Char
  deriving (Eq, Ord, Show)

-- Example of Listable type class
class Listable a where
  toList :: a -> [Int]

-- Declare instances Int and Bool as instances of the Listable type class
instance Listable Int where
  -- toList :: Int -> [Int]
  toList x = [x]

instance Listable Bool where
  -- toList :: Bool -> [Int]
  toList True = [1]
  toList False = [0]

-- data Tree a = Empty
--             | Node a (Tree a) (Tree a)
--
-- instance Listable (Tree Int) where
--   toList Empty = []
--   toList (Node x l r) = (toList l) ++ [x] ++ (toList r)

-- We can put type class constraints on instance declarations:
instance (Listable a, Listable b) => Listable (a, b) where
  toList (x, y) = toList x ++ toList y
