module DNA (toRNA) where

import qualified Data.Map as Map

-- | if string contains invalid character, return Nothing
-- | if string contains only valid nucleotides, return Just transcription

dnas = zip "CGTA" "GCAU"

toRNA :: String -> Maybe String
toRNA str = sequence $ map mapTo str
  where mapTo = flip lookup $ dnas