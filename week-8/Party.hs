{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Party where

import Employee
import Data.Tree
import Data.List


-- Exercise 1
-- Adds Employee to the GuestList and updates the cached Fun value
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e : es) (fun + empFun e)

-- Make GuestList a monoid
instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL x a) (GL y b) = GL (x ++ y) (a + b)

-- From the given lists return the one with more fun
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2
-- Implement treeFold for Data.Tree (with no Empty val?)
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x xs) = f x $ map (treeFold f) xs

-- test the treeFold
companyFun :: Tree Employee -> Fun
companyFun = treeFold (\x xs -> empFun x + sum xs)

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e glPairs = (withBoss, withoutBoss)
  where
    withBoss = glCons e $ mconcat (map snd glPairs)
    withoutBoss = mconcat $ map (uncurry moreFun) glPairs

-- Exercise 4
-- From the given hierarchy give the GuestList with more fun
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

main :: IO ()
main = do
  contents <- readFile "company.txt"
  let companyTree = read contents :: Tree Employee
      GL guests fun = maxFun companyTree

  putStrLn $ "Total fun: " ++ show fun
  mapM_ putStrLn . sort . map empName $ guests
