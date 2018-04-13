{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative
import           Control.Monad (void)

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1
-- Implement a Functor instance for Parser using first
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap f (Parser fp) = Parser $ fmap (first f) . fp


-- Exercise 2
-- Implement an Applicative instance for Parser
instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)

  pfun <*> pval = Parser $ \s ->
    case runParser pfun s of
      Nothing -> Nothing
      Just (f, ss) -> runParser (f <$> pval) ss


-- Exercise 3
-- Create a parser which expects to see the characters ’a’ and ’b’ and returns
-- them as a pair.
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- Create a parser which acts in the same way as abParser but returns () instead
-- of the characters ’a’ and ’b’.
abParser_ :: Parser ()
abParser_ = f <$> char 'a' <*> char 'b'
  where f _ _ = ()

-- Create a parser which reads two integer values separated
-- by a space and returns the integer values in a list.
intPair :: Parser [Integer]
intPair = f <$> posInt <*> char ' ' <*> posInt
  where f x _ y = [x, y]


-- Exercise 4
-- Write an "Alternative" instance for Parser
--   empty represents the parser which always fails.
--   p1 <|> p2 represents the parser which first tries running p1.
--     If p1 succeeds then p2 is ignored and the result of p1 is returned.
--     Otherwise, if p1 fails, then p2 is tried instead.
instance Alternative Parser where
  empty     = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s -> runParser p1 s <|> runParser p2 s


-- Exercise 5
-- Implement a Parser () which parses either an integer value or an uppercase
-- character, and fails otherwise.
intOrUppercase :: Parser ()
intOrUppercase = (void posInt) <|> void (satisfy isUpper)
