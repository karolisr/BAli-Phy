{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Enum (Enum,
                      enumFrom,
                      enumFromThen,
                      enumFromTo,
                      enumFromThenTo)
    where

import Data.Ord      -- for <=
import Compiler.Num  -- for -,+
import Data.Bool     -- for otherwise

class Enum a where { }
-- succ :: a -> a
-- pred :: a -> a
-- toEnum :: Int -> a
-- fromEnum :: a -> Int
-- enumFrom :: a -> [a]
-- enumFromThen :: a -> a -> [a]
-- enumFromTo   :: a -> a -> [a]
-- enumFromThenTo :: a -> a -> a -> [a]

-- This isn't a standard function -- I made it up..
enumByFrom by from = from:enumByFrom by (from+by)

-- This isn't right for negative "by"
enumByToFrom by to from | from <= to    = from:enumByToFrom by to (from+by)
                        | otherwise     = []

succ n = n + 1

pred n = n - 1

enumFrom n = n:enumFrom (succ n)

enumFromThen from next = enumByFrom (next-from) from

enumFromThenTo from next to = enumByToFrom (next - from) to from

enumFromTo n m | n <= m    = n:enumFromTo (succ n) m
               | otherwise = []

