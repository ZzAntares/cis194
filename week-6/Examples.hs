{-# OPTIONS_GHC -Wall #-}

module Examples where

-- Strict evaluation implementation for logical AND
(&&!) :: Bool -> Bool -> Bool
True &&! True = True
True &&! False = False
False &&! True = False
False &&! False = False
