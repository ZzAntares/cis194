data IntList = Empty
             | Cons Int IntList
  deriving Show

-- Common patterns arise
addOneToAll :: IntList -> IntList
addOneToAll Empty = Empty
addOneToAll (Cons n rest) = Cons (n + 1) (addOneToAll rest)

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons n rest) = Cons (abs n) (absAll rest)

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons n rest) = Cons (n^2) (squareAll rest)

-- Abstract the common parts
mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

addOne x = x + 1
square x = x * x

exampleList = Cons (-1) $ Cons 2 $ Cons (-6) Empty

-- Filtering is another common pattern
keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
  | even x = Cons x (keepOnlyEven xs)
  | otherwise = keepOnlyEven xs

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList f (Cons x xs)
  | f x = Cons x (filterIntList f xs)
  | otherwise = filterIntList f xs

-- Summarizing pattern
findMax :: IntList -> Int
findMax Empty = 0
findMax (Cons x xs)
  | x >= (findMax xs) = x
  | otherwise = findMax xs

findMin :: IntList -> Int
findMin Empty = 0
findMin (Cons x xs)
  | x <= (findMin xs) = x
  | otherwise = findMin xs

findSum :: IntList -> Int
findSum Empty = 0
findSum (Cons x xs) = x + findSum xs

findProduct :: IntList -> Int
findProduct Empty = 1
findProduct (Cons x xs) = x * findProduct xs

reduce :: (Int -> Int -> Int) -> IntList -> Int
reduce _ Empty = 0
reduce f (Cons x xs) = f x $ reduce f xs

maxVal = reduce (\a b -> if a > b then a else b) exampleList
sumList = reduce (+) exampleList

-- Polymorphic data types
data List t = E
            | C t (List t)
  deriving Show

lst1 :: List Int
lst1 = C 3 $ C 5 $ C 2 E

lst2 :: List Char
lst2 = C 'x' $ C 'y' $ C 'z' E

lst3 :: List Bool
lst3 = C True $ C False E

-- Polymorphic functions
filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList p (C x xs)
  | p x       = C x $ filterList p xs
  | otherwise = filterList p xs

mapList :: (t -> a) -> List t -> List a
mapList _ E = E
mapList f (C x xs) = C (f x) $ mapList f xs

-- Making head a total function
safeHead :: [t] -> Maybe t
safeHead [] = Nothing
safeHead (x : _) = Just x
