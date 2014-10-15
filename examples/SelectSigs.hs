
import           Control.Monad (when)
import qualified Data.ByteString.Char8 as BS
import           Data.List (elemIndex, intercalate)
import           Data.Maybe (isNothing, catMaybes, fromJust)
import           System.Environment (getArgs)

import Data.Avc

main = do
    args <- getArgs
    when (null args) $ error "Usage SelectSigs file.avc sig1 sig2 ..."
    let filename:sigNames = args
    statements <- parseAvcFile filename
    -- TODO: catch exception when no FORMAT found
    let format = head $ filter isFormat statements
    let midxs = getSigIdxs format (map BS.pack sigNames)
    when (Nothing `elem` midxs) $ error $ "Signals not found in format: " ++ nameMissingSignals midxs sigNames
    let idxs = catMaybes midxs
    let sigs = map (Sig . BS.pack) sigNames
    -- BS.putStrLn $ toString $ Format sigs CNil
    mapM (BS.putStrLn . toString) $ selectSigs idxs statements

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
