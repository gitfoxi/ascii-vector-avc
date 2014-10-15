{-# LANGUAGE OverloadedStrings #-}

-- | Help print Avc types

module Data.Avc.Encode (toString) where

import qualified Data.ByteString.Char8 as BS
import           Data.Monoid ((<>))

import Data.Avc.Type

-- TODO: There's probably some generic way to do this for any sort of String /
-- ByteString / Text.

toString :: Statement -> BS.ByteString
toString (Repeat rep devcyc vec comment) =
    BS.unwords["R" <> BS.pack (show rep)
              , devcyc
              , vec
              , ";"
              , commentToString comment]
toString (Format sigs comment) =
    BS.unwords $ ["FORMAT"] ++ map sigToString sigs ++ [";", commentToString comment]
toString (EOF) = ""

sigToString :: Signal -> BS.ByteString
sigToString (Sig name) = name
sigToString (SigGroup name) = name

commentToString :: Comment -> BS.ByteString
commentToString (Comment c) = c
commentToString CNil = ""
