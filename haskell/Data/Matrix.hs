module Data.Matrix where

import Foreign.Vector

import Text.Show

fromLists xss = fromVectors $ list_to_vector $ map list_to_vector xss

foreign import bpcall "Matrix:transpose" tr :: Matrix a -> Matrix a
foreign import bpcall "Matrix:" scaleMatrix :: a -> Matrix a -> Matrix a
foreign import bpcall "Matrix:elementwise_multiply" (%*%) :: Matrix Double -> Matrix Double -> Matrix Double
foreign import bpcall "Prelude:show" showMatrix :: Matrix a -> CPPString

instance Show (Matrix a) where
    show x = unpack_cpp_string $ showMatrix x
