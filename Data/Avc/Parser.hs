{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

-- TODO: Hackage, github, haddock, tests
-- TODO: isGzipped to a personal utility package; readAndDecompress
--       like readFile but if Gzipped will also decompress
-- TODO: Only works with parseOnly beause I decided to do special handling of
-- putting this into statements so trailing space at the end of the file will
-- make it return "Partial _"
-- TODO: keep track of line numbers in case of failure

module Data.Avc.Parser (parseAvcFile) where

import Codec.Compression.GZip
import qualified Data.ByteString.Lazy.Char8 as BL

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics hiding (Rep)

import Data.ByteString.Char8 as BS hiding (split)
import Control.Monad
import Control.Applicative hiding (many)

import Data.Attoparsec.ByteString.Char8 as A
import qualified System.IO as IO

import qualified Test.HUnit as H

import qualified Data.List as DL hiding (words, unwords, takeWhile, elem, lines)
import Data.Function (on)

import Data.List.Split (keepDelimsR, whenElt, split)

import Prelude hiding (lines, elem, null, unwords, map, null)
import qualified Prelude as P

import Data.Char as DC (isLetter, isDigit)

import Data.Binary.Get (runGet, getWord16le)

import Data.Avc.Type

-- | Parse a simple AVC which may be gzipped
-- Only handles simple files with one character per state like:
--
-- # Begin AVC
-- FORMAT a b c;
-- R10 10X ; vector comment # other comment
-- R10 01H ;
parseAvcFile :: FilePath -> IO [Statement]
parseAvcFile filename = do
    content <- decompressFile filename
    let strict = BL.toStrict content
    let ls = BS.lines strict
    let ncmnt = nocomment ls
    let stmts = joinStatements ncmnt
    return $ getStatements stmts

-- TODO: put this in my utility module
decompressFile :: FilePath -> IO BL.ByteString
decompressFile filename = do
    bs <- BL.readFile filename
    isgz <- isGzipped bs
    let decompressed = decompress bs
    return $ if isgz then decompressed else bs

isGzipped :: BL.ByteString -> IO Bool
isGzipped bs = do
    let magic = runGet getWord16le bs
    return $ magic == 0x8b1f
-- /utility

-- | Strip # comments from a list of lines
nocomment :: [ByteString] -> [ByteString]
nocomment = P.map dropComment
    where dropComment bs = let idx = elemIndex '#' bs in
                           case idx of
                            Nothing -> bs
                            Just i -> BS.take i bs

-- | Join statements terminated by ';' into one line each also keeping
-- info after ';' which is a comment on the statement.
joinStatements :: [ByteString] -> [ByteString]
joinStatements bss = P.map unwords (grouped bss)
    where grouped = split (keepDelimsR $ whenElt hasSemi)
          hasSemi hs = ';' `elem` hs

-- Assume first statement is Format
getFormat :: ByteString -> Statement
getFormat bs =
    case parseOnly (skipSpace *> parseFormat) bs of
                    Left e -> error ("Error parsing FORMAT: " ++ show e ++ "\nInput:" ++ show bs)
                    Right r -> r

getStatements :: [ByteString] -> [Statement]
getStatements = P.map getStatement

-- getStatement "R1 cyc 123" -- TODO: doesn't return. some kind of inifinite loop without the ';'
getStatement bs =
    case parseOnly (skipSpace *> parseStatement) bs of
                    Left e -> error ("Error parsing FORMAT: " ++ show e ++ "\nInput:" ++ show bs)
                    Right r -> r

parseStatement :: Parser Statement
parseStatement =
    choice [parseRepeat
           ,parseFormat
           ,parseEof]

parseEof :: Parser Statement
parseEof = do
    endOfInput
    return EOF

-- Parser A is like parser b but it skips trailing spaces
-- TODO: returns "Partial _" if space until EOF
lexeme :: Parser a -> Parser a
lexeme pa = pa <* skipSpace
{-# INLINE lexeme #-}

parseRepeat :: Parser Statement
parseRepeat = Repeat <$> (keyword "R" *> rep) <*> devcyc <*> parseVec <*> parseComment
    where
        rep = lexeme decimal
        devcyc = lexeme $ takeWhile1 isVecChar
{-# INLINE parseRepeat #-}

isVecChar :: Char -> Bool
isVecChar c = A.isAlpha_ascii c || A.isDigit c
{-# INLINE isVecChar #-}

parseVec :: Parser ByteString
parseVec = do
    -- This was 38% of time and 52% of allocation
    -- vec <- manyTill (skipSpace *> A.takeWhile isVecChar) (skipSpace *> semicolon)
    -- return $ BS.concat vec
    -- This is slightly better. Not a huge win and maybe too haphazard
    vec <- A.takeWhile (/= ';') <* semicolon
    return $ BS.filter (/= ' ') vec
{-# INLINE parseVec #-}


parseFail :: String -> ByteString -> [String] -> String -> Statement
parseFail usrmsg trying contexts errmsg = error message
    where message = "Fail parsing: " ++ unpack trying
                 ++ "\nError message: " ++ errmsg
                 ++ "\nContexts: " ++ DL.intercalate "\n+" contexts

parseLeftover :: String -> ByteString -> Statement
parseLeftover trying leftover = error $ "Leftover input trying: " ++ trying ++ " Leftover: '" ++ unpack leftover ++ "'"


parseFormat' :: Parser Statement
parseFormat' = Format <$> (keyword "FORMAT" *> sigs) <*> parseComment <?> "FORMAT"
    where 
        sigs = manyTill parseSig semicolon

parseFormat :: Parser Statement
parseFormat = do
    linespaces
    parseFormat'

parseSig :: Parser Signal
parseSig = Sig <$> lexeme (takeWhile1 isSigChar)

letter = letter_ascii

parseComment :: Parser Comment
parseComment = do
    cmnt <- A.takeWhile (const True) <* endOfInput
    return $ Comment cmnt

linespaces :: Parser ()
linespaces = void $ A.takeWhile (`elem` "\t ")

isSigChar :: Char -> Bool
isSigChar c = isLetter c || DC.isDigit c || c `elem` "[]:_"

semicolon :: Parser ()
semicolon = void $ lexeme $ char ';'

keyword :: ByteString -> Parser ()
keyword kw = void $ lexeme $ string kw
{-# INLINE keyword #-}

