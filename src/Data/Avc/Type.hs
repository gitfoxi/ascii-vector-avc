
module Data.Avc.Type
    ( Signal(..)
    , Comment(..)
    , Statement(..)
    ) where

import Data.ByteString.Char8 as BS hiding (split)

type Rep = Int
type DevCyc = ByteString
type Vec = ByteString  -- TODO: Vector
type Name = ByteString

-- | A Signal has a name
data Signal = Sig Name
            | SigGroup Name -- TODO: need pin-config file to implement
            deriving (Show, Eq)

-- | May be useful to keep vector comments
data Comment = Comment ByteString
             | CNil
             deriving (Show, Eq)

-- | A statement is a "FORMAT a b c;" definition or a "RXX devcyc vecdata; comment"
data Statement = Repeat Rep DevCyc Vec Comment
               | Format [Signal] Comment
               | EOF
               deriving (Show, Eq)

-- I forget what this is for
-- instance NFData Signal where rnf = genericRnf
-- instance NFData Comment where rnf = genericRnf
-- instance NFData Statement where rnf = genericRnf

