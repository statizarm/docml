{-# LANGUAGE FlexibleContexts #-}
module Language.Parser (
    parse
) where

import Language.Token
import Language.AST

import Prelude hiding (until)

import Text.Parsec hiding (parse)
import Text.Parsec.Char
import Data.Functor

import Data.Map (fromList)

text :: Stream s m Char => ParsecT s u m (AST LabeledNodeData)
text = textNode <$> many1 (satisfy (\x -> x /= '{' && x /= '}'))

body :: Stream s m Char => ParsecT s u m [AST LabeledNodeData]
body = many (node <|> text)

tag :: Stream s m Char => ParsecT s u m String 
tag = spaces $> getIdentifier <*> identifier 

args :: Stream s m Char => ParsecT s u m [(String, String)]
args = option [] $ try $ spaces *> between argumentOpenBrace argumentCloseBrace (commaSep keyValue)
    where
        commaSep p = sepBy p (spaces *> char ',' *> spaces)

keyValue :: Stream s m Char => ParsecT s u m (String, String)
keyValue = (,) <$> key <* eq <*> value
    where
        key = spaces $> getIdentifier <*> identifier
        value = spaces $> getValue <*> valueToken
        eq = spaces <* equals

node :: Stream s m Char => ParsecT s u m (AST LabeledNodeData)
node = between blockOpenBrace blockCloseBrace node'
    where
        node' = labelNode <$> tag <*> args <*> body

parse :: Stream s m Char => s -> m (Either ParseError (AST LabeledNodeData))
parse = runParserT (RootNode <$> body) () ""
