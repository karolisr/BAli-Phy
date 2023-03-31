module Probability.Probability where

import Numeric.Log
import Data.Ratio

{- We need to handle numbers that are > 1 in the following cases:
     1/2   (2 > 1)
     1-3*p (3 > 1)
   So, we assume that IOdds z = 1 / Odds z -}

-- We could keep track of HOW MANY zeros!
-- ... | Zero Int | ...
-- Then we could penalize multiplying by an additional zero.
data Probability = Zero | Odds Double | One | IOdds Double | Infinity

complement Zero     = One
complement (Odds y) = (Odds (-y))
complement One      = Zero
complement _        = error "complement: not a probability"

fromProb :: Probability -> Double
fromProb Zero                 = 0
fromProb (Odds y) | y < 0     = let e = exp y in e / (1 + e)
fromProb (Odds y) | otherwise = 1 / (1 + exp(-y))
fromProb One                  = 1
fromProb (IOdds y)| y < 0     = let e = exp y in (1 + e) /e
                  | otherwise = (1 + exp(-y))
fromProb Infinity             = 1 / 0

mkProb :: Double -> Probability
mkProb p | p < 0     = error "Negative Probability!"
         | p == 0    = Zero
         | p < 1     = Odds $ log $ p / (1-p)
         | p == 1    = One
         | p > 1     = let (Odds y) = mkProb (1/p) in IOdds y
--       | p == Inf  = Infinity

-- Only defined on non-zero probabilities.
logProb :: Probability -> Double
logProb Zero                 = error "Probability: log(0)"
logProb (Odds y) | y < 0     = y - log1p (exp y)
                 | otherwise = -log1p (exp (-y))
logProb One                  = 0
logProb (IOdds y)            = -logProb (Odds y)
logProb Infinity             = 1/0

expToProb :: Double -> Probability
expToProb z | z < 0     = Odds $ z - log1p (-exp z)
            | z == 0    = One
            | z > 0     = let Odds z2 = expToProb (-z) in IOdds z2

plus Zero      x        = x
plus x         Zero     = x
plus Infinity  x        = Infinity
plus x         Infinity = Infinity
plus (Odds y1) (Odds y2) | y12 > 0    = mkProb (fromProb x1 + fromProb x2)
                         | y12 == 0   = One
                         | y12 > (-1) = One - mkProb (expm1 (y12) / (1 + exp y1) / (1 + exp y2) )
                         | otherwise      = mkProb $ fromProb (Odds y1) + fromProb (Odds y2)
                         where y12 = y1 + y2
plus (IOdds y1) (Odds y2) = plus (Odds y2) (IOdds y1)
plus x1         x2        = mkProb (fromProb x1 + fromProb x2)


sub x        Zero        = x
sub Infinity Infinity    = error "Inf - Inf is undefined"
sub Infinity _           = Infinity
sub One      One         = Zero
sub One      (Odds y)   = Odds (-y)
sub p1@(Odds y1) p2@(Odds y2) | y1 >= y2 = p1 * (One - (p2/p1))
sub (IOdds y1) (Odds y2) = mkProb (fromProb (IOdds y1) - fromProb (Odds y2))
sub (IOdds y1) (IOdds y2) | y1 == y2 = Zero
                          | y1 <  y2 = mkProb (fromProb (IOdds y1) - fromProb (IOdds y2))
sub _         _ = error "Negative probability"

-- Done!
mul Zero      Infinity  = error "0 * Inf is undefined"
mul Zero      x         = Zero
mul x         Zero      = Zero
mul One       x         = x
mul x         One       = x
mul Infinity  x         = Infinity
mul x         Infinity  = Infinity
mul (Odds y1) (Odds y2) | y1 > y2   = Odds $ y2 - log1p( exp(y2-y1) + exp(-y1) )
                        | otherwise = Odds $ y1 - log1p( exp(y1-y2) + exp(-y2) )
mul (Odds y1) (IOdds y2) | y1 == y2 = One
                         | y1 > y2   = Odds $ y2 - log1p( exp(y2-y1) + exp(-y1) )
                         | otherwise = Odds $ y1 - log1p( exp(y1-y2) + exp(-y2) )

mul (IOdds y1) (Odds y2) = mul (Odds y2) (IOdds y1)
mul (IOdds y1) (IOdds y2) = let (Odds y3) = mul (Odds y1) (Odds y2) in IOdds y3

instance Eq Probability where
    (Odds y1)  == (Odds y2)  = y1 == y2
    (IOdds y1) == (IOdds y2) = y1 == y2
    x          == y          = pord x == pord y

pord Zero      = 0
pord (Odds _)  = 1
pord One       = 2
pord (IOdds _) = 3
pord Infinity  = 4

instance Ord Probability where
    (Odds y1) < (Odds y2) = y1 < y2
    (IOdds y1) < (IOdds y2) = y1 > y2
    x < y  = pord x < pord y

instance Num Probability where
    (+) = plus
    (-) = sub
    (*) = mul
    abs = id
    negate = error "Can't negate a probability"
    signum Zero = 0
    signum _    = 1
    fromInteger 0 = Zero
    fromInteger 1 = One
    fromInteger x | x < 0     = error "Negative Probability!"
                  | otherwise = mkProb $ fromInteger x

instance Real Probability where
    toRational Zero = 0 % 1
    toRational (Odds y) = toRational $ fromProb (Odds y)
    toRational One  = 1 % 1
    toRational (IOdds y) = toRational $ fromProb (IOdds y)
    toRational Infinity = 1 % 0

instance Fractional Probability where
    recip Zero      = Infinity
    recip (Odds y)  = IOdds y
    recip One       = One
    recip (IOdds y) = Odds y
    recip Infinity  = Zero

    fromRational x = mkProb x

instance Show Probability where
    show Zero = "0"
    show (Odds y) = show $ fromProb (Odds y)
    show One = "1"
    show (IOdds y) = show $ fromProb (IOdds y)
    show Infinity = "Inf"

-- It WOULD be nice if we could write logOdds y = log (p/(1-p))
logOdds (Odds y) = y
logOdds p = logProb p - logProb (1-p)

-- Problem:
-- complement (1/3)  ==> "3 is not a probability"
