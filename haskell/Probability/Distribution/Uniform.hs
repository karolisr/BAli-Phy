module Probability.Distribution.Uniform where

import Probability.Random
import MCMC

builtin "Distribution:uniform_density" uniform_density 3
builtin "Distribution:sample_uniform" builtin_sample_uniform 3

uniform_bounds l u = between l u
uniform_effect l u x = add_move $ slice_sample_real_random_variable x (uniform_bounds l u)
sample_uniform l u = RandomStructure (uniform_effect l u) modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_uniform l u s)))

uniform l u = Distribution "uniform" (make_densities $ uniform_density l u) () (sample_uniform l u) (uniform_bounds l u)

builtin "Distribution:uniform_int_density" uniform_int_density 3
builtin "Distribution:sample_uniform_int" builtin_sample_uniform_int 3

uniform_int_bounds l u = integer_between l u
uniform_int_effect l u x = add_move $ slice_sample_integer_random_variable x (uniform_int_bounds l u)
sample_uniform_int l u = RandomStructure (uniform_int_effect l u) modifiable_structure $ liftIO (IOAction (\s->(s,builtin_sample_uniform_int l u s)))

uniform_int l u = Distribution "uniform_continuous" (make_densities $ uniform_int_density l u) () (sample_uniform_int l u) (integer_between l u)
