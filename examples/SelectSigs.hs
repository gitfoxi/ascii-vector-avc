
import           Control.Monad (when)
import qualified Data.ByteString.Char8 as BS
import           Data.List (elemIndex, intercalate)
import           Data.Maybe (isNothing, catMaybes, fromJust)
import           System.Environment (getArgs)

import Data.Avc
import Data.Avc.Util

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
    mapM (BS.putStrLn . toString) $ selectSigs idxs statements

