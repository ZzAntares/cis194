{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Sized
import Editor
import Buffer
import Data.Monoid
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1
-- Write an append function for JoinList that yields a new JoinList whose
-- monoidal annotation is derived from those of the two arguments.
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

-- Helper function: Get the annotation at the root of a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m


-- Exercise 2
-- Write an indexJ function to get the anotation at the given index with O(log n)
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ i (Append m left right)
  | i > rootSize = Nothing -- Index is greater than the total size
  | i > leftSize = indexJ (i - leftSize) right
  | otherwise = indexJ i left
  where
    rootSize = getSize . size $ m
    leftSize = getSize . size . tag $ left
indexJ _ _ = Nothing


--  Drops the first n elements from a JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 list = list
dropJ n list@(Append m left right)
  | n < 0          = list
  | n >=  rootSize = Empty
  | n > leftSize   = dropJ (n - leftSize) right
  | otherwise      = dropJ n left +++ right
  where
    rootSize = getSize . size $ m
    leftSize = getSize . size . tag $ left
dropJ _ _ = Empty


-- Returns the first n elements of a JoinList, dropping all other elements.
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n el@(Single _ _)
  | n > 0 = el
takeJ n list@(Append m left right)
  | n < 0 = Empty
  | n >= rootSize = list
  | n > leftSize = left +++ takeJ (n - leftSize) right
  | otherwise = takeJ n left
  where
    rootSize = getSize . size $ m
    leftSize = getSize . size . tag $ left
takeJ _ _ = Empty

-- Test for Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

ex3 :: JoinList Score String
ex3 = scoreLine "yay " +++ scoreLine "haskell!"

-- Exercise 4
instance (Monoid a, Monoid b) => Monoid (JoinList (a, b) String) where
  mempty = Empty
  mappend = (+++)

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ a) = a
  toString (Append _ left right) = toString left ++ toString right

  fromString = mconcat . map (\e -> Single (scoreString e, Size 1) e) . lines

  line = indexJ

  numLines = getSize . size . tag

  replaceLine i msg buf
    | i > numLines buf = buf
    | otherwise = takeJ i buf +++ fromString msg +++ dropJ (i + 1) buf

  value = getScore . fst . tag
    where getScore (Score i) = i

main :: IO()
main = runEditor editor (fromString "Haskell" :: JoinList (Score, Size) String)
