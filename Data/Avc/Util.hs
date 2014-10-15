
module Data.Avc.Util ( selectSigs
                     , getSigIdxs
                     , nameMissingSignals
                     , isFormat) where

import qualified Data.ByteString.Char8 as BS
import           Data.List (elemIndex, intercalate)
import           Data.Maybe (isNothing, catMaybes, fromJust)

import Data.Avc.Type

-- | Utilities for working with AVC files

selectSigs :: [Int] -> [Statement] -> [Statement]
selectSigs idxs = map selectSigs'
  where
    selectSigs' (Format sigs comment)
        = Format (map (sigs !!) idxs) comment
    selectSigs' (Repeat rep cyc vec comment)
        = Repeat rep cyc
            (BS.pack (map (vec `BS.index`) idxs))
            comment
    selectSigs' EOF = EOF

nameMissingSignals :: [Maybe Int] -> [String] -> String
nameMissingSignals mi ss = let z = zip mi ss
                               f = filter (isNothing . fst) z
                               ms = map snd f
                           in intercalate ", " ms

getSigIdxs :: Statement -> [BS.ByteString] -> [Maybe Int]
getSigIdxs format
    = map (`elemIndex` sigNames format)
  where
    sigNames (Format sigs _) = map sigName sigs
    sigName (Sig name) = name

isFormat (Format{}) = True
isFormat _ = False
