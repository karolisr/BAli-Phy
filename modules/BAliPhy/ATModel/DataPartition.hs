module BAliPhy.ATModel.DataPartition where

import Data.Maybe
import Tree
import Alignment

data IModel
data SModel
data HMM
data PairwiseAlignment

data Partition = Partition {
      smodel :: SModel,
      imodel :: Maybe IModel,
      scale  :: Double,
      get_tree :: Tree,
      get_alignment :: AlignmentOnTree,
      hmms :: Maybe (Array Int HMM)
    }
