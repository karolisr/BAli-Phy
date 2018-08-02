module SModel.ReversibleFrequency where
{
import Alphabet;
data ReversibleFrequency = ReversibleFrequency a b c d;
builtin builtin_plus_gwf 3 "plus_gwF" "SModel";

simple_smap a = iotaUnsigned (alphabetSize a);

-- pi is a vector double here
plus_f_matrix a pi = plus_gwf_matrix a pi 1.0;

-- pi is a vector double here
plus_gwf_matrix a pi f = builtin_plus_gwf a f pi;

-- pi is [Double] here
plus_gwf a pi f = ReversibleFrequency a (simple_smap a) pi' (plus_gwf_matrix a pi' f) where {pi' = list_to_vector pi};
plus_f a pi = plus_gwf a pi 1.0;

uniform_frequencies a = replicate n $ 1.0/(intToDouble n) where {n = alphabetSize a};

uniform_frequencies_dict a = zip (alphabet_letters a) (uniform_frequencies a);

plus_f_equal_frequencies a = plus_f a (uniform_frequencies a);

-- pi is [(String,Double)] here
select_element key dict = case lookup key dict of {Just value -> value;
                                                   Nothing    -> error $ "Can't find element " ++ show key ++ " in dictionary!"};

select_elements keys dict = map (flip select_element dict) keys;

get_ordered_elements xs xps plural = if length xs == length xps
                                     then select_elements xs xps
                                     else error $ "Expected "++show (length xs)++" "++plural
                                              ++" but got "++ show (length xps)++"!";

frequencies_from_dict a pi = get_ordered_elements (alphabet_letters a) pi "frequencies";

plus_f' a pi = plus_f a pi' where {pi' = frequencies_from_dict a pi};
plus_gwf' a pi f = plus_gwf a pi' f where {pi' = frequencies_from_dict a pi};
}
