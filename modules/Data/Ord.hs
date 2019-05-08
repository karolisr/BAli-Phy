{-# LANGUAGE NoImplicitPrelude #-}
module Data.Ord (module Data.Ord,
                 module Data.Eq)
where

import Data.Eq

data Ordering = EQ | LT | GT

builtin > 2 "greaterthan" "Prelude"
builtin >= 2 "greaterthanorequal" "Prelude"
builtin < 2 "lessthan" "Prelude"
builtin <= 2 "lessthanorequal" "Prelude"

infix 4 <, <=, >, >=

min x y = if (x <= y) then x else y
max x y = if (x >= y) then x else y

compare x y | x < y  = LT
compare x y | x > y  = GT
compare x y | x == y = EQ
