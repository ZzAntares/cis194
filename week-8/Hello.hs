{-# OPTIONS_GHC -Wall #-}

module Hello where

-- main :: IO ()
-- main = putStrLn "Hello, Haskell!" >> putStrLn "From Hello.hs"

main :: IO ()
main = putStrLn "Please enter a number: "
  >> (readLn >>= (\n -> putStrLn (show (n + 1))))
