{-# LANGUAGE NoImplicitPrelude #-}
module Data.Ord (module Data.Eq,
                 Ord,
                (<),
                (<=),
                (>),
                (>=),
                compare,
                min,
                max)
where

import Data.Eq

data Ordering = EQ | LT | GT

instance Eq Ordering where
    EQ == EQ = True
    LT == LT = True
    GT == GT = True
    _  == _  = False

infix 4 <, <=, >, >=

class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<), (>), (>=), (<=) :: a -> a -> Bool
    min, max :: a -> a -> a

    min x y = if (x <= y) then x else y
    max x y = if (x >= y) then x else y

    compare x y | x <  y    = LT
                | x == y    = EQ
                | otherwise = GT

    x <  y = not (x >= y)
    x >  y = not (x <= y)
    x >= y = x > y || x == y
    x <= y = x < y || x == y

foreign import bpcall "Prelude:" lessthan_char :: Char -> Char -> Bool
foreign import bpcall "Prelude:" lessthan_int :: Int -> Int -> Bool
foreign import bpcall "Prelude:" lessthan_integer :: Integer -> Integer -> Bool
foreign import bpcall "Prelude:" lessthan_double :: Double -> Double -> Bool

instance Ord Char where
    (<) = lessthan_char

instance Ord Int where
    (<) = lessthan_int 

instance Ord Integer where
    (<) = lessthan_integer

instance Ord Double where
    (<) = lessthan_double

instance Ord a => Ord [a] where
    compare []     []      = EQ
    compare []     (_:_)   = LT
    compare (_:_)  []      = GT
    compare (x:xs) (y:ys)  = case compare x y of LT -> LT
                                                 GT -> GT
                                                 EQ -> compare xs ys
    x < y = compare x y == LT
    x > y = compare x y == GT

instance (Ord a, Ord b) => Ord (a,b) where
    compare (x1,y1) (x2,y2) = let c1 = compare x1 x2
                              in case c1 of
                                   EQ -> compare y1 y2
                                   _  -> c1
