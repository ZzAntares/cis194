-- Exercise 1
---- Transform a number into a list consisting of its digits.
toDigits :: Integer -> [Integer]
toDigits n
  | div n 10 == 0 = [n]
  | otherwise = (toDigits (div n 10)) ++ (toDigits (mod n 10))

---- Transform a number into a list consisting of its digits in reverse order.
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n


-- Exercise 2
-- Iterates a list by two items and passes as parameters to the given function
mapTwoR :: (Integer -> Integer -> [Integer]) -> [Integer] -> [Integer]
mapTwoR _ []          = []
mapTwoR _ (x:[])      = [x]
mapTwoR func (x:y:zs) = func x y ++ mapTwoR func zs

-- Starting by the right doubles every even item in the list
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = reverse $ mapTwoR (\x y -> [x, y * 2]) (reverse list)


-- Exercise 3
-- Sum all the digits of the given integers in the list
sumDigits :: [Integer] -> Integer
sumDigits list = sum $ map (\x -> sum $ toDigits x) list

-- Exercise 4
validate :: Integer -> Bool
validate n =
  (== 0)
  . (`mod` 10)
  . sumDigits
  . doubleEveryOther
  . toDigits
  $ n
