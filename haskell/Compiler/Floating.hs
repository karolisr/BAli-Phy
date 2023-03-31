{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Floating where

import Compiler.Error
import Compiler.Num -- for LogDouble
import Compiler.Fractional
import Compiler.Integral -- for quot, rem
import Data.Function -- for (.)
import Data.Ord

infixl 8 **

class Fractional a => Floating a where { }
-- pi :: a
-- exp, sqrt, log :: a -> a
-- (**), logBase :: a -> a -> a
-- sin, tan, cos :: a -> a
-- asin, atan, acos :: a -> a
-- sinh, tanh, cosh :: a -> a
-- asinh, atanh, acosh :: a -> a

pi = 3.14159265358979323846
foreign import bpcall "Real:exp" exp :: Double -> Double
foreign import bpcall "Real:sqrt" sqrt :: Double -> Double
foreign import bpcall "Real:log" log :: Double -> Double
foreign import bpcall "Real:pow" (**) :: a -> a -> a
foreign import bpcall "Real:logBase" logBase :: Double -> Double -> Double
foreign import bpcall "Real:sin" sin :: Double -> Double
foreign import bpcall "Real:tan" tan :: Double -> Double
foreign import bpcall "Real:cos" cos :: Double -> Double
foreign import bpcall "Real:asin" asin :: Double -> Double
foreign import bpcall "Real:atan" atan :: Double -> Double
foreign import bpcall "Real:acos" acos :: Double -> Double
foreign import bpcall "Real:sinh" sinh :: Double -> Double
foreign import bpcall "Real:tanh" tanh :: Double -> Double
foreign import bpcall "Real:cosh" cosh :: Double -> Double
foreign import bpcall "Real:asinh" asinh :: Double -> Double
foreign import bpcall "Real:atanh" atanh :: Double -> Double
foreign import bpcall "Real:acosh" acosh :: Double -> Double

foreign import bpcall "Prelude:expToLogDouble" expToLogDouble :: Double -> LogDouble

infixr 8 `pow`
foreign import bpcall "Real:pow" pow :: LogDouble -> Double -> LogDouble


instance Floating Double
-- Question: does implementing this type class slow down the code?


instance Floating LogDouble
-- The problem is that only (sqrt, pow) and maybe (exp) really make sense for LogDouble.
-- (log) can return negatives, as can many of the other functions.


