{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Tokenizer where

import Text.Parsec
import Text.Parsec.Char

import Data.Functor
import Data.Functor.Identity (Identity)

data Token = Identifier {getIdentifier :: String}
           | ValueToken {getValue :: String}
           | BlockOpenBrace
           | BlockCloseBrace
           | ArgumentOpenBrace
           | ArgumentCloseBrace
           | Equals
    deriving(Show, Eq)

identifier :: Stream s m Char => ParsecT s u m Token
identifier = Identifier <$> ((:) <$> letter <*> many alphaNum)

valueToken :: Stream s m Char => ParsecT s u m Token
valueToken = ValueToken <$> (quotedString <|> fmap getIdentifier identifier)
    where
        quotedString = char '"' *> manyTill anyChar (char '"')

equals :: Stream s m Char => ParsecT s u m Token
equals = char '=' $> Equals

blockOpenBrace :: Stream s m Char => ParsecT s u m Token
blockOpenBrace = char '{' $> BlockOpenBrace

blockCloseBrace :: Stream s m Char => ParsecT s u m Token
blockCloseBrace = char '}' $> BlockOpenBrace

argumentOpenBrace :: Stream s m Char => ParsecT s u m Token
argumentOpenBrace = char '[' $> ArgumentOpenBrace

argumentCloseBrace :: Stream s m Char => ParsecT s u m Token
argumentCloseBrace = char ']' $> ArgumentCloseBrace
