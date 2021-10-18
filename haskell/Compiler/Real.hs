{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Real where

import Compiler.Base
import Compiler.Num
import Compiler.Enum -- for class Enum
import Data.Ord      -- for <
import Data.Function -- for .

infixl 8 ^, ^^, **
infixl 7 /, `quot`, `rem`, `div`, `mod`

class Num a => Fractional a where { }
-- (/) :: a -> a -> a
-- recip :: a -> a
-- fromRational :: Rational -> a

class (Num a, Ord a) => Real a where { }
--    toRational :: a -> Rational

class (Real a, Enum a) => Integral a  where { }
-- quot :: a -> a -> a
-- rem  :: a -> a -> a
-- div  :: a -> a -> a
-- mod  :: a -> a -> a
-- quotRem :: a -> a -> (a,a)
-- divMod  :: a -> a -> (a,a)
-- toInteger :: a -> Integer

builtin / 2 "divide" "Prelude"
builtin div 2 "div" "Prelude"
builtin mod 2 "mod" "Prelude"
builtin quot 2 "quot" "Prelude"
builtin rem 2 "rem" "Prelude"

class Fractional a => Floating a where { }
-- pi :: a
-- exp, sqrt, log :: a -> a
-- (**), logBase :: a -> a -> a
-- sin, tan, cos :: a -> a
-- asin, atan, acos :: a -> a
-- sinh, tanh, cosh :: a -> a
-- asinh, atanh, acosh :: a -> a

pi = 3.14159265358979323846
builtin exp 1 "exp" "Real"
builtin sqrt 1 "sqrt" "Real"
builtin log 1 "log" "Real"
builtin ** 2 "pow" "Real"
builtin logBase 2 "logBase" "Real"
builtin sin 1 "sin" "Real"
builtin tan 1 "tan" "Real"
builtin cos 1 "cos" "Real"
builtin asin 1 "asin" "Real"
builtin atan 1 "atan" "Real"
builtin acos 1 "acos" "Real"
builtin sinh 1 "sinh" "Real"
builtin tanh 1 "tanh" "Real"
builtin cosh 1 "cosh" "Real"
builtin asinh 1 "asinh" "Real"
builtin atanh 1 "atanh" "Real"
builtin acosh 1 "acosh" "Real"

class (Real a, Fractional a) => RealFrac a where
    properFraction :: (Integral b) => a -> (b,a)
--    truncate, round  :: (Integral b) => a -> b
--    ceiling, floor   :: (Integral b) => a -> b

builtin truncate 1 "truncate" "Prelude"
builtin ceiling 1 "ceiling" "Prelude"
builtin floor 1 "floor" "Prelude"
builtin round 1 "round" "Prelude"

builtin doubleToInt 1 "doubleToInt" "Prelude"

builtin expToLogDouble 1 "expToLogDouble" "Prelude"
builtin doubleToLogDouble 1 "doubleToLogDouble" "Prelude"
builtin intToDouble 1 "intToDouble" "Prelude"

-- We need == to use GHC's code directly
x0 ^ y0 | y0 < 0 = error("Negative exponent")
x ^ 1 = x
x ^ n = x*(x^(n-1))

recip y = 1.0/y -- should be 1/y

x ^^ n = if n >= 0 then x^n else recip (x^(negate n))
