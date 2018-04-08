{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Examples where

data Tree a = Empty
            | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty


-- Function to compute the size of a tree (number of Nodes)
treeSize :: Tree a -> Integer
treeSize Empty = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

-- Function to sum the data in a tree of Integers
treeSum :: Tree Integer -> Integer
treeSum Empty = 0
treeSum (Node l x r) = x + treeSum l + treeSum r

-- Function to get the depth of a tree
treeDepth :: Tree a -> Integer
treeDepth Empty = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

-- Flat tree elements into a list
flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- Get the max element of the tree
treeMax :: (Ord a, Bounded a) => Tree a -> a
treeMax Empty = minBound
treeMax (Node l x r) = treeMax l `max` x `max` treeMax r

-- See patterns and define a generalized function to fold the tree
treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold z _ Empty = z
treeFold z f (Node l x r) = f (treeFold z f l) x (treeFold z f r)

-- Define above functions but using treeFold
treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\lres _ rres -> 1 + lres + rres)

treeSum' :: Tree Integer -> Integer
treeSum' = treeFold 0 (\l x r -> x + l + r)

treeDepth' :: Tree a -> Integer
treeDepth' = treeFold 0 (\l _ r -> 1 + max l r)

flatten' :: Tree a -> [a]
flatten' = treeFold [] (\l x r -> l ++ [x] ++ r)

treeMax' :: (Ord a, Bounded a) => Tree a -> a
treeMax' = treeFold minBound (\l x r -> l `max` x `max` r)

-- ExprT type from Homework 5
data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add xpa xpb) = eval xpa + eval xpb
eval (Mul xpa xpb) = eval xpa * eval xpb


-- Fold for ExprT
exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold f _ _ (Lit x) = f x
exprTFold f g h (Add xpa xpb) = g (exprTFold f g h xpa) (exprTFold f g h xpb)
exprTFold f g h (Mul xpa xpb) = h (exprTFold f g h xpa) (exprTFold f g h xpb)

eval' :: ExprT -> Integer
eval' = exprTFold id (+) (*)

-- Count number of literals in an expression
numLiterals :: ExprT -> Integer
numLiterals = exprTFold (const 1) (+) (+)

class Monoid' m where
  mempty' :: m
  mappend' :: m -> m -> m
  mconcat' :: [m] -> m
  mconcat' = foldr mappend' mempty'

instance Monoid' [a] where
  mempty' = []
  mappend' = (++)

-- Integers form monoids with multiplication and addition
-- We can't create multiple instances for the same Integer type
-- So we create new types for Monoid Integer Addition
newtype Sum a = Sum a deriving (Eq, Ord, Num, Show)

getSum :: Sum a -> a
getSum (Sum x) = x

instance Num a => Monoid' (Sum a) where
  mempty' = Sum 0
  mappend' = (+)

-- And for Monoid Integer Product
newtype Product a = Product a deriving (Eq, Ord, Num, Show)

getProduct :: Product a -> a
getProduct (Product a) = a

instance Num a => Monoid' (Product a) where
  mempty' = Product 1
  mappend' = (*)

-- Product of a list of Integers using mconcat
lst :: [Integer]
lst = [1, 5, 8, 23, 423, 99]

prod :: Integer
prod = getProduct . mconcat' . map Product $ lst

-- Pairs form a monoid as long as the components are monoids
instance (Monoid' a, Monoid' b) => Monoid' (a, b) where
  mempty' = (mempty', mempty')
  mappend' (a, b) (c, d) = (mappend' a c, mappend' b d)

-- Challenge: can you make an instance of Monoid for Bool?
newtype Or a = Or Bool deriving (Show, Eq)
instance Monoid' (Or a) where
  mempty' = Or False
  mappend' (Or x) (Or y) = Or $ x || y

newtype And a = And Bool deriving (Show, Eq)
instance Monoid' (And a) where
  mempty' = And True
  mappend' (And x) (And y) = And $ x && y

-- Challenge: how would you make function types an instance of Monoid
-- Not quite sure but ...
instance (Monoid' a, Monoid' b) => Monoid' (a -> b) where
  mempty' _ = mempty'
  mappend' f g x = mappend' (f x) (g x)
