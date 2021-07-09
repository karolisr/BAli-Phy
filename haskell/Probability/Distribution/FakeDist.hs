module Probability.Distribution.FakeDist where

import Probability.Random
import Tree
import SModel
import BAliPhy.ATModel

fake_sample_error = error "sampling from fake_dist is not allowed.  You can only observe it."
fake_range_error = error "fake_dist has no range.  You can only observe it."

annotated_fake_dist_0_pr tree alignment smodel sequences = do
  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel
  let subst_root = modifiable (numNodes tree - 1)

  let (transition_ps, cls, anc_seqs, likelihood) = observe_partition_type_0 tree alignment smodel sequences subst_root

  property "subst_root" subst_root
  property "transition_ps" transition_ps
  property "cond_likes" cls
  property "anc_seqs" anc_seqs
  property "likelihood" likelihood
  return $ [likelihood]

fake_dist_0 tree alignment smodel =
    Distribution "fake_dist_0" (annotated_fake_dist_0_pr tree alignment smodel) (no_quantile "fake_dist_0") fake_sample_error fake_range_error

annotated_fake_dist_1_pr tree smodel sequences = do
  in_edge "tree" tree
  in_edge "smodel" smodel
  let subst_root = modifiable (numNodes tree - 1)

  let (transition_ps, cls, anc_seqs, likelihood) = observe_partition_type_1 tree smodel sequences subst_root

  property "subst_root" subst_root
  property "transition_ps" transition_ps
  property "cond_likes" cls
  property "anc_seqs" anc_seqs
  property "likelihood" likelihood
  return $ [likelihood]

fake_dist_1 tree smodel =
    Distribution "fake_dist_1" (annotated_fake_dist_1_pr tree smodel) (no_quantile "fake_dist_1") fake_sample_error fake_range_error
