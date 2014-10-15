{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

-- TODO: Hackage, git

module Data.Avc where

import Codec.Compression.GZip
import qualified Data.ByteString.Lazy.Char8 as BL (readFile, lines, toStrict)

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

avcfile :: String
avcfile = "bist_l2c_m24r1s2d12_dll_pp__010_20140828.avc.gz"
avcfile2 = "bist_l2c_m24r1s2d12_dll_pp__010_20140828.avc"

type Rep = Int
type DevCyc = ByteString
type Vec = ByteString  -- TODO: Vector
type Name = ByteString

data Signal = Sig Name
            | SigGroup Name -- TODO: need pin-config file to implement
            deriving (Show, Eq, Generic)

data Comment = Comment ByteString
             | CNil
             deriving (Show, Eq, Generic)

data Statement = Repeat Rep DevCyc Vec Comment
               | Format [Signal] Comment
               | EOF
               deriving (Show, Eq, Generic)

instance NFData Signal where rnf = genericRnf
instance NFData Comment where rnf = genericRnf
instance NFData Statement where rnf = genericRnf

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

parseRepeat :: Parser Statement
parseRepeat = Repeat <$> (keyword "R" *> rep) <*> devcyc <*> parseVec <*> parseComment
    where
        rep = decimal <* skipSpace
        devcyc = takeWhile1 isVecChar <* skipSpace

isVecChar :: Char -> Bool
isVecChar c = A.isAlpha_ascii c || A.isDigit c
{-# INLINE isVecChar #-}

parseVec :: Parser ByteString
parseVec = do
    vec <- manyTill (skipSpace *> A.takeWhile isVecChar) (skipSpace *> semicolon)
    return $ BS.concat vec


getStatements :: [ByteString] -> [Statement]
getStatements = P.map getStatement


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
        sigs = manyTill (skipSpace *> parseSig <* skipSpace) semicolon

parseFormat :: Parser Statement
parseFormat = do
    linespaces
    parseFormat'

parseSig :: Parser Signal
parseSig = Sig <$> takeWhile1 isSigChar

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
semicolon = void $ char ';'

keyword :: ByteString -> Parser ()
keyword kw = void $ string kw *> spaces

spaces :: Parser ()
spaces = do
    A.takeWhile isSpace
    return ()

