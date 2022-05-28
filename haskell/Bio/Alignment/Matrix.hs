module Bio.Alignment.Matrix where

import Bio.Sequence
import Bio.Alphabet
import Data.BitVector

data AlignmentMatrix

foreign import bpcall "Alignment:alignment_length" alignment_length :: AlignmentMatrix -> Int

foreign import bpcall "Alignment:load_alignment" builtin_load_alignment :: Alphabet -> CPPString -> AlignmentMatrix

load_alignment :: Alphabet -> String -> AlignmentMatrix
load_alignment alphabet filename = IOAction (\s -> (s, builtin_load_alignment alphabet (list_to_string filename)))

foreign import bpcall "Alignment:alignment_from_sequences" builtin_alignment_from_sequences :: Alphabet -> EVector Sequence -> AlignmentMatrix

alignment_from_sequences :: Alphabet -> [Sequence] -> AlignmentMatrix
alignment_from_sequences a seqs = builtin_alignment_from_sequences a (list_to_vector seqs)

foreign import bpcall "Alignment:sequences_from_alignment" builtin_sequences_from_alignment :: AlignmentMatrix -> EVector Sequence

sequences_from_alignment :: AlignmentMatrix -> [ Sequence ]
sequences_from_alignment a = list_from_vector $ builtin_sequences_from_alignment a

foreign import bpcall "Alignment:sequence_names" builtin_sequence_names :: AlignmentMatrix -> EVector CPPString

sequence_names :: AlignmentMatrix -> [String]
sequence_names a = map unpack_cpp_string $ list_from_vector $ builtin_sequence_names a

foreign import bpcall "Alignment:reorder_alignment" builtin_reorder_alignment :: EVector Sequence -> AlignmentMatrix -> AlignmentMatrix

reorder_alignment :: [String] -> AlignmentMatrix -> AlignmentMatrix
reorder_alignment names a = builtin_reorder_alignment names' a where names' = list_to_vector $ map pack_cpp_string names

foreign import bpcall "Bits:alignment_row_to_presence_bitvector" builtin_alignment_row_to_bitvector :: () -> () -> ()
alignment_row_to_bitvector a row = BitVector $ builtin_alignment_row_to_bitvector a row

