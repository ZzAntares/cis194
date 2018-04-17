{-# OPTIONS_GHC -Wall #-}

module Examples where

import Control.Applicative

type Name = String

data Employee = Employee { getName :: Name
                         , getPhone :: String
                         } deriving Show


-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
--
--   -- Every Applicative is also a Functor
--   -- So it's possible to define fmap in terms of pure and apply.
--   fmap g x = pure g <*> x
--
--
-- instance Applicative [] where
--   pure x = [x]
--   [] <*> _ = []
--   -- fs <*> xs = [f x | f <- fs, x <- xs]
--   (f:fs) <*> as = (map f as) ++ (fs <*> as)


names :: [Name]
names = ["Joe", "Sara", "Mae"]

phones :: [String]
phones = ["555-5555", "123-456-7890", "555-4321"]

employees1 :: [Employee]
employees1 = Employee <$> names <*> phones

(.+) :: (Num c, Applicative f) => f c -> f c -> f c
(.+) = liftA2 (+) -- addition lifted to some Applicative context

(.*) :: (Num c, Applicative f) => f c -> f c -> f c
(.*) = liftA2 (*) -- multiplication lifted to some Applicative context

n :: [Integer]
n = ([4, 5] .* pure 2) .+ [6, 1] -- (either 4 or 5) times 2, plus either 6 or 1

m1 :: Maybe Integer
m1 = (Just 3 .+ Just 5) .* Just 8

m2 :: Maybe Integer
m2 = (Just 3 .+ Nothing) .* Just 8

newtype MyZipList a = MyZipList { getMyZipList :: [a] }
  deriving (Eq, Show)

instance Functor MyZipList where
  -- fmap g (MyZipList xs) = MyZipList $ map g xs
  -- fmap g zxs = MyZipList $ map g (getMyZipList zxs)
  fmap g = MyZipList . map g . getMyZipList

instance Applicative MyZipList where
  -- pure x = MyZipList (repeat x)
  pure = MyZipList . repeat
  -- MyZipList fs <*> MyZipList xs = MyZipList $ zipWith (\f x -> f x) fs xs
  -- MyZipList fs <*> MyZipList xs = MyZipList $ zipWith ($) fs xs
  MyZipList fs <*> MyZipList xs = MyZipList (zipWith ($) fs xs)

employees2 :: [Employee]
employees2 = getMyZipList $ Employee <$> MyZipList names <*> MyZipList phones


-- instance Functor ((->) e) where
--   -- fmap g fx = g . fx
--   fmap = (.)
--
-- instance Applicative ((->) e) where
--   -- pure x = \_ -> x
--   -- pure x = const x
--   pure = const
--
--   -- fg <*> fx = \a -> f x where
--   --   f = fg a
--   --   x = fx a
--   f <*> x = \e -> (f e) (x e)

data BigRecord = BR { getRName        :: Name
                    , getSSN          :: String
                    , getSalary       :: Integer
                    , getRPhone       :: String
                    , getLicensePlate :: String
                    , getNumSickDays  :: Int
                    }

record :: BigRecord
record = BR "Brent" "XXX-XX-XXX4" 600000000 "555-1234" "JGX-55T3" 2

getEmp :: BigRecord -> Employee
-- getEmp r = Employee (getRName r) (getRPhone r)
getEmp = Employee <$> getRName <*> getRPhone

ex01 :: Employee
ex01 = getEmp record

pair :: Applicative f => f a -> f b -> f (a, b)
-- pair fa fb = (\x y -> (x, y)) <$> fa <*> fb
-- pair fa fb = (,) <$> fa <*> fb
-- pair fa fb = liftA2 (,) fa fb
pair = liftA2 (,)

(.*>) :: Applicative f => f a -> f b -> f b
(.*>) = liftA2 (flip const)
-- Just 3 .*> Just 5 -- Just 5
-- [1, 2, 3] .*> [4, 5, 6] -- [4, 5, 6, 4, 5, 6, 4, 5, 6]
-- getZipList $ (ZipList [1, 2, 3]) .*> (ZipList [4, 5, 6]) -- [4, 5, 6]
-- getLine .*> getLine -- IO computation that yields the 2nd result

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
-- mapA f xs = foldr (liftA2 (:)) (pure []) (fmap f xs)
mapA f xs = sequenceA' $ fmap f xs
-- mapA (pure . even) [1, 2, 3] -- [False, True, False]

sequenceA' :: Applicative f => [f a] -> f [a]
--sequenceA' [] = pure []
--sequenceA' (x:xs) = liftA2 (:) x $ sequenceA xs
sequenceA' = foldr (liftA2 (:)) (pure [])

replicateA :: Applicative f => Int -> f a -> f [a]
-- replicateA i = liftA2 replicate $ pure i
-- replicateA i = fmap (replicate i)
replicateA = fmap . replicate
-- replicateA 3 $ Just 4 -- Just [4, 4, 4]
-- replicateA 3 [1, 2, 3] -- [[1,1,1],[2,2,2],[3,3,3]]
-- getZipList $ replicateA 3 $ ZipList [1, 2, 3] -- [[1,1,1],[2,2,2],[3,3,3]]
