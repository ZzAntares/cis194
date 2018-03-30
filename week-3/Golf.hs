{-# OPTIONS_GHC -Wall #-}

module Golf where

-- Exercise 1
-- takeEvery takes an integer N which denotes the periodicity of items in the
-- given list to pick. For example takeEvery 2 [1, 2, 3, 4] will take [2, 4]
-- whereas takeEvery 3 [1, 2, 3, 4] will take only [3].
takeEvery :: Int -> [a] -> [a]
-- First we need to know the index of each element in the list, for that we use
-- zip function, this generates a list of pairs composed by the index and the
-- element of the list at that position. The resulting list passes to the filter
-- function which will preserve only those elements in which the index of such
-- element is divided exactly by the N parameter, this in turn keeps only the
-- elments in which we are intersted in. Finally the resulting list will pass to
-- the map function which will apply the snd function on each pair giving us
-- only the element part of each pair and thus discarding the index which is no
-- longer needed.
takeEvery n = map snd . filter (\(i, _) -> mod i n == 0) . zip [1..]

-- The output of skips is a list of lists. The first list in the output should
-- be the same as the input list. The second list in the output should
-- contain every second element from the input list... and the nth list in
-- the output should contain every nth element from the input list.
skips :: [a] -> [[a]]
-- The map function will apply takeEvery to the list as many items there are on
-- the list. Because the index of the element is the N we need to pass to
-- takeEvery we first zip the given list so we have a list of pairs consisting
-- of the index and the element at that index.
skips as = map (\(i, _) -> takeEvery i as) (zip [1..] as)

-- Exercise 2
-- Takes the elements by triplets and returns the middle number if this one is
-- greater than the number to its left and right. These "maximas" are returned
-- in a list.
-- Examples:
--   localMaxima [2,9,5,6,1] == [9,6]
--   localMaxima [2,3,4,1,5] == [4]
--   localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
-- Use pattern matching to retrieve the first 3 elements and the rest of the list.
-- Using guards we check if the condition "the middle number is larger" is met,
-- if it is then we insert this number at the begginin of the list formed
-- recursively by the localMaxima function. If the condition fails then we just
-- compute the list using recursion but without inserting an element to the list.
localMaxima (x : y : z : s)
  | x < y && y > z = y : localMaxima (y : z : s)
  | otherwise      = localMaxima (y : z : s)
-- When the pattern matching does not apply, that is, there are only 2 or fewer
-- elements then we return an empty list.
localMaxima _ = []


-- Exercise 3

-- Find occurrences of a number in a list
occurrences :: Integer -> [Integer] -> Integer
occurrences needle = toInteger . length . filter (== needle)

-- Build a list of the occurrences searching from 0 to 9
build :: [Integer] -> [Integer]
build haystack = map (`occurrences` haystack) [0..9]

-- Build the string representation of the given numerical row. That is, put ' '
-- where there are zeros and '*' where non-zero.
-- Example: buildHist [0,3,0,0,0,1,0,0,0,0] == " *   *    "
buildRow :: [Integer] -> String
buildRow = map (\x -> if x == 0 then ' ' else '*')

-- Build the histogram given the top row
-- Take a single row of integers and parse it into a string, appending such
-- string to the given list of strings. (This needs haskell stylish!)
buildHist :: [Integer] -> [String] -> String
buildHist [0, 0, 0, 0, 0, 0, 0, 0, 0, 0] strs = unlines strs
buildHist list strs =
  buildHist (map (\x -> if x == 0 then 0 else x - 1) list) (buildRow list : strs)

-- Create a full histogram in string format from the given list of integers
-- Example: histogram [3, 5] == " * * \n==========\n0123456789\n"
histogram :: [Integer] -> String
histogram list = buildHist (build list) ["==========", "0123456789"]
