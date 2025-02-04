module Bio.Sequence where

import Data.Map as Map hiding (map)
import Bio.Alphabet
import Data.Text (Text)
import qualified Data.Text as T
import Data.BitVector

-- Dummy type that stands for the c++ `sequence` type
-- Can we eliminate this?
data ESequence = ESequence
foreign import bpcall "Alignment:sequence_name" builtin_sequence_name :: ESequence -> CPPString
foreign import bpcall "Alignment:" sequenceDataRaw :: ESequence -> CPPString

data Sequence = Sequence { sequenceName, sequenceData :: Text }
mkSequence :: ESequence -> Sequence
mkSequence s = Sequence (T.fromCppString $ builtin_sequence_name s) (T.fromCppString $ sequenceDataRaw s)

foreign import bpcall "Alignment:sequence_to_indices" builtin_sequence_to_indices :: Alphabet -> CPPString -> EVector Int
foreign import bpcall "Alignment:sequenceToAlignedIndices" builtin_sequenceToAlignedIndices :: Alphabet -> CPPString -> EVector Int
sequence_to_indices a (Sequence _ s) = builtin_sequence_to_indices a (T.toCppString s)
sequenceToAlignedIndices a (Sequence _ s) = builtin_sequenceToAlignedIndices a (T.toCppString s)

-- sequence_to_indices :: Sequence -> [Int]
-- maybe add this later

foreign import bpcall "Alignment:" statesToLetters :: EVector Int -> EVector Int -> EVector Int

foreign import bpcall "Alignment:load_sequences" builtin_load_sequences :: CPPString -> IO (EVector ESequence)
load_sequences :: String -> IO [Sequence]
load_sequences filename = fmap (fmap mkSequence . list_from_vector) $ builtin_load_sequences (list_to_string filename)

foreign import bpcall "Alignment:getRange" builtin_getRange :: CPPString -> Int -> EVector Int
foreign import bpcall "Alignment:select_range" builtin_select_range :: EVector Int -> CPPString -> CPPString
select_range :: String -> [Sequence] -> [Sequence]
select_range range sequences = let maxLength = maximum [ T.length $ sequenceData s | s <- sequences ]
                                   range' = builtin_getRange (list_to_string range) maxLength
                                   select (Sequence name chars) = Sequence name (T.fromCppString $ builtin_select_range range' (T.toCppString chars))
                               in fmap select sequences

reorder_sequences names sequences | length names /= length sequences  = error "Sequences.reorder_sequences: different number of names and sequences!"
                                  | otherwise = [ sequences_map Map.! name | name <- names ]
    where sequences_map = Map.fromList [ (sequenceName sequence, sequence) | sequence <- sequences ]

sequence_length a sequence = vector_size $ sequence_to_indices a sequence

get_sequence_lengths a sequences = Map.fromList [ (sequenceName sequence, sequence_length a sequence) | sequence <- sequences]

foreign import bpcall "Likelihood:" bitmask_from_sequence :: EVector Int -> CBitVector
foreign import bpcall "Likelihood:" strip_gaps :: EVector Int -> EVector Int
foreign import bpcall "Likelihood:" maskSequenceRaw :: CBitVector -> EVector Int -> EVector Int

bitmask_from_sequence' s = BitVector $ bitmask_from_sequence s
maskSequence (BitVector bv) sequence = maskSequenceRaw bv sequence

fastaSeq (Sequence label seq) = T.concat [T.singleton '>', label, T.singleton '\n', seq, T.singleton '\n']

fastaSeqs sequences = T.concat [fastaSeq s | s <- sequences]

