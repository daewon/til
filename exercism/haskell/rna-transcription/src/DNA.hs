module DNA (toRNA) where

import qualified Data.Map as Map

-- | if string contains invalid character, return Nothing
-- | if string contains only valid nucleotides, return Just transcription

dnas = zip "CGTA" "GCAU"

toRNA :: (Traversable t) => t Char -> Maybe (t Char)
toRNA str = traverse find str
  where find = flip lookup $ dnas
