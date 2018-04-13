{-# OPTIONS_GHC -Wall #-}

module Examples where

import Control.Applicative ( ZipList(..)
                           , liftA2
                           )

type Name = String

data Employee = Employee { getName :: Name
                         , getPhone :: String
                         } deriving Show

-- Make an Employee out of Maybe arguments
employeeLifter :: (Name -> String -> Employee) -> Maybe Name -> Maybe String -> Maybe Employee
employeeLifter f (Just name) (Just phone) = Just $ f name phone
employeeLifter _ _ _ = Nothing

-- Make a list of Employee out of a list of names and phones (with combinations)
employeesLifter :: (Name -> String -> Employee) -> [Name] -> [String] -> [Employee]
employeesLifter f names phones = [ f name phone | name <- names
                                                , phone <- phones ]

-- Make a list of Employee out of a list of names and phones (with zip like behaviour)
employeesLifter' :: (Name -> String -> Employee) -> [Name] -> [String] -> [Employee]
employeesLifter' = zipWith

-- Make an Employee out of functions that return the name and phone
employeeLifterF :: (Name -> String -> Employee) -> (e -> Name) -> (e -> String) -> e -> Employee
employeeLifterF f readName readPhone e = f (readName e) (readPhone e)

-- Type of the Employee constructor:
--   Employee :: Name -> String -> Employee

class Functor f => MyApplicative f where
  pure :: a -> f a
  (.<*>) :: f (a -> b) -> f a -> f b

-- instance MyApplicative Maybe where
--   pure = Just
--   Nothing .<*> _ = Nothing
--   _ .<*> Nothing = Nothing
--   Just f .<*> Just x = Just (f x)

instance MyApplicative Maybe where
  pure = Just
  Nothing .<*> _ = Nothing
  Just f .<*> x = fmap f x

mName1, mName2 :: Maybe Name
mName1 = Nothing
mName2 = Just "Brent"

mPhone1, mPhone2 :: Maybe String
mPhone1 = Nothing
mPhone2 = Just "555-1234"

ex01, ex02, ex03, ex04 :: Maybe Employee
ex01 = Employee <$> mName1 <*> mPhone1
ex02 = Employee <$> mName1 <*> mPhone2
ex03 = Employee <$> mName2 <*> mPhone1
ex04 = Employee <$> mName2 <*> mPhone2

main :: IO ()
main = print ex01
  >> print ex02
  >> print ex03
  >> print ex04

instance MyApplicative [] where
  pure x = [x]
  fs .<*> xs = [ f x | f <- fs, x <- xs ]

-- Other examples from LYAH
instance MyApplicative IO where
  pure = return
  a .<*> b = do
    f <- a
    x <- b
    return (f x)

instance MyApplicative ((->) r) where
  pure = const
  f .<*> g = \x -> f x (g x)

instance MyApplicative ZipList where
  pure x = ZipList (repeat x)
  (ZipList fs) .<*> (ZipList xs) = ZipList $ zipWith (\f x -> f x) fs xs

sequenceAf :: Applicative f => [f a] -> f [a]
sequenceAf = foldr (liftA2 (:)) (Prelude.pure [])
