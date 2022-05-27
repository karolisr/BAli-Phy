module MCMC where

import Foreign.Pair
import Foreign.Vector
import Range

builtin builtin_register_transition_kernel 2 "MCMC:register_transition_kernel"
register_transition_kernel rate move = IOAction (\s -> (s,builtin_register_transition_kernel rate move))

-- Transition kernel: Perform gibbs sampling on modifiable x, which takes values [0..n-1], in context c
builtin builtin_gibbs_sample_categorical 4 "MCMC:gibbs_sample_categorical"
gibbs_sample_categorical x n c = IOAction (pair_from_c . builtin_gibbs_sample_categorical x n c)

builtin builtin_discrete_uniform_avoid_mh 5 "MCMC:discrete_uniform_avoid_mh"
discrete_uniform_avoid_mh x low high c = IOAction (pair_from_c . builtin_discrete_uniform_avoid_mh x low high c)

-- It would be nice if we could (i) seq x and bounds HERE, and (ii) convert range to bounds HERE.
-- But the seq needs to be done during changeable execution, and we execute the IO unchangeably.
builtin builtin_inc_dec_mh 4 "MCMC:inc_dec_mh"
inc_dec_mh x bnds c = IOAction (pair_from_c . builtin_inc_dec_mh x bnds c)

builtin builtin_slice_sample_real_random_variable 4 "MCMC:slice_sample_real_random_variable"
slice_sample_real_random_variable x bnds c = IOAction (pair_from_c . builtin_slice_sample_real_random_variable x (c_range bnds) c)

builtin builtin_slice_sample_integer_random_variable 4 "MCMC:slice_sample_integer_random_variable"
slice_sample_integer_random_variable x bnds c = IOAction (pair_from_c . builtin_slice_sample_integer_random_variable x (c_range bnds) c)

builtin builtin_walk_tree_path 2 "MCMC:walk_tree_path"
walk_tree_path tree c = vector_to_list $ builtin_walk_tree_path tree c

-- This is "unsafe" because it doesn't update alignments
builtin builtin_nni_on_branch_unsafe 3 "MCMC:NNI_on_branch_unsafe"
nni_on_branch_unsafe tree branch c = IOAction (\s->(s,builtin_nni_on_branch_unsafe tree branch c))

-- This is "unsafe" because it doesn't update alignments
builtin builtin_tnni_on_branch_unsafe 3 "MCMC:TT_NNI_on_branch_unsafe"
tnni_on_branch_unsafe tree branch c = IOAction (\s->(s,builtin_tnni_on_branch_unsafe tree branch c))

-- This is "unsafe" because it doesn't update alignments
builtin builtin_fnpr_unsafe_proposal 4 "MCMC:FNPR_unsafe"
fnpr_unsafe_proposal tree node c = IOAction (pair_from_c . builtin_fnpr_unsafe_proposal tree node c)

walk_tree_sample_nni_unsafe tree c = sequence_ [ nni_on_branch_unsafe tree branch c | branch <- walk_tree_path tree c]

builtin builtin_walk_tree_sample_alignments 3 "MCMC:walk_tree_sample_alignments"
walk_tree_sample_alignments tree c = IOAction (pair_from_c . builtin_walk_tree_sample_alignments tree c)

builtin builtin_realign_from_tips 3 "MCMC:realign_from_tips"
realign_from_tips tree c = IOAction (pair_from_c . builtin_realign_from_tips tree c)

builtin builtin_walk_tree_sample_NNI 3 "MCMC:walk_tree_sample_NNI"
walk_tree_sample_NNI tree c = IOAction (pair_from_c . builtin_walk_tree_sample_NNI tree c)

builtin builtin_walk_tree_sample_NNI_and_A 3 "MCMC:walk_tree_sample_NNI_and_A"
walk_tree_sample_NNI_and_A tree c = IOAction (pair_from_c . builtin_walk_tree_sample_NNI_and_A tree c)

builtin builtin_walk_tree_sample_NNI_and_branch_lengths 3 "MCMC:walk_tree_sample_NNI_and_branch_lengths"
walk_tree_sample_NNI_and_branch_lengths tree c = IOAction (pair_from_c . builtin_walk_tree_sample_NNI_and_branch_lengths tree c)

builtin builtin_walk_tree_sample_branch_lengths 3 "MCMC:walk_tree_sample_branch_lengths"
walk_tree_sample_branch_lengths tree c = IOAction (pair_from_c . builtin_walk_tree_sample_branch_lengths tree c)

builtin builtin_sample_SPR_all 3 "MCMC:sample_SPR_all"
sample_SPR_all tree c = IOAction (pair_from_c . builtin_sample_SPR_all tree c)

builtin builtin_sample_SPR_nodes 3 "MCMC:sample_SPR_nodes"
sample_SPR_nodes tree c = IOAction (pair_from_c . builtin_sample_SPR_nodes tree c)

builtin builtin_sample_SPR_flat 3 "MCMC:sample_SPR_flat"
sample_SPR_flat tree c = IOAction (pair_from_c . builtin_sample_SPR_flat tree c)

builtin builtin_copy_context 2 "MCMC:copy_context"
copy_context c = IOAction (pair_from_c . builtin_copy_context c)

builtin builtin_release_context 2 "MCMC:release_context"
release_context c = IOAction (pair_from_c . builtin_release_context c)

builtin builtin_switch_to_context 3 "MCMC:switch_to_context"
switch_to_context c1 c2  = IOAction (pair_from_c . builtin_switch_to_context c1 c2)

builtin builtin_accept_MH 4 "MCMC:accept_MH"
accept_MH c1 c2 ratio  = IOAction (pair_from_c . builtin_accept_MH c1 c2 ratio)

-- TODO: What if copy_context returns a Box<context>?
--       Then if memory is tight, we would destroy the context object, and release the  context.
--       This might take a while though if garbage-collection didn't happen immediately.
--       And we might need to pivot back to c2 later to release it later.


-- Proposal = Context -> IO LogDouble
data Proposal
data Context

metropolis_hastings :: Proposal -> Context -> IO Bool
metropolis_hastings proposal c1 = do
  c2 <- copy_context c1
  ratio <- proposal c2
  accept <- accept_MH c1 c2 ratio
  if accept then switch_to_context c1 c2 else return ()
  release_context c2
  return accept

