module Language.AST (
    AST (..)
) where


import qualified Data.Map.Lazy as Map

data AST = LabelNode {
    getLabel :: String,
    getArgs :: Map.Map String String,
    getChildren :: [AST]
} | TextNode {
    getText :: String
} | RootNode {
    getChildren :: [AST]
} deriving(Show)
