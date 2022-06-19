module Tool.Link.Check where

import System.IO

import Control.Monad.State
import Control.Monad.Writer

import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Map as Map

import Language.AST
import Language.SymbolTable

import Tool.Link.Options
import Tool.Link.Error
import Tool.Link.Symbol


joinSymTab :: (LabeledNodeData, SymbolTable Symbol) -> State (SymbolTable Symbol) ()
joinSymTab (d, tab) = let
    in do
        oldTab <- get
        put $ Map.unionWith (++) oldTab tab


astSymbol :: (String -> Bool) -> AST LabeledNodeData -> Identity (SymbolTable Symbol)
astSymbol isSymbol ast@(LabelNode v c) = let
        l = getLabel v
    in
        if isSymbol l then
            return $ makeSymbolTable v $ foldr getSymbol "" (rootNode c)
        else
            return mempty
    where
        getSymbol :: LabeledNodeData -> String -> String
        getSymbol n s = let
                label = getLabel n
            in if label == textNodeLabel then
                    unwords $ filter (not . null) [s, (getText . getData) n]
                else 
                    s


getSymbolTable :: (String -> Bool) -> AST LabeledNodeData -> SymbolTable Symbol
getSymbolTable f ast = let
        ast' = runIdentity . localSymbolTable (astSymbol f) $ ast
        ast'' = PostOrderAST ast'
    in execState (mapM_ joinSymTab ast'') mempty


checkLinks isSymbol isLink ast = let 
        st = getSymbolTable (\x -> isSymbol x || isLink x) ast
        log = execWriter $ Map.traverseWithKey check st
        check :: String -> Symbol -> Writer [LinkError] ()
        check k s = let 
                linkSym = head . filter (isLink . getSymbolLabel) $ s
            in when (any (isLink . getSymbolLabel) s) $ do
                if all (isLink . getSymbolLabel) s then
                    tell [unknownSymbolError k linkSym]
                else when (length (filter (isSymbol . getSymbolLabel) s) > 1) $ do
                    tell [multipleIdentifiers k linkSym $ filter (isSymbol . getSymbolLabel) s]
    in if log == mempty then Right st else Left log


runLinkTool :: (String -> Maybe String) -> LinkOptions -> AST LabeledNodeData -> IO ()
runLinkTool f options = either (printMultipleErrors h f) printResult . checkLinks isSymbol isLink
    where
        h = stderr
        printResult = print
        isSymbol = flip elem $ getIdentifierTags options
        isLink = flip elem $ getLinkTags options
