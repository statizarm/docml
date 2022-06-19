module Tool.Unique.Validation (
    runUniqueTool,
    getSymbolTable,
) where


import Control.Monad.State
import Control.Monad.Writer

import System.IO

import qualified Data.Map.Lazy as Map
import Data.Semigroup (Semigroup)

import Language.AST
import Language.SymbolTable

import Tool.Unique.Error (UniqueError (UniqueError), printMultipleUniqueErrors)
import Tool.Unique.Options (UniqueOptions (UniqueOptions, getIdentifierTags))
import Tool.Unique.Symbol (makeSymbolTable)


updateSymTab :: (LabeledNodeData, SymbolTable SymbolAttributes) -> StateT (SymbolTable SymbolAttributes) (Writer [UniqueError]) ()
updateSymTab (d, tab) = let
        createError :: SymbolTable SymbolAttributes -> SymbolTable SymbolAttributes -> String -> UniqueError
        createError oldTab newTab name = let
                message = name
                oldPos = getSymbolPosition $ oldTab Map.! name
                newPos = getSymbolPosition $ newTab Map.! name
            in UniqueError message newPos oldPos
    in do
        oldTab <- get
        let int = oldTab `Map.intersection` tab
        if null int then
            put $ oldTab <> tab
        else
            tell $ foldr ((:) . createError oldTab tab) [] (Map.keys int)


astSymbol :: (String -> Bool) -> AST LabeledNodeData -> Writer [UniqueError] (SymbolTable SymbolAttributes)
astSymbol isSymbol ast@(LabelNode v c) = let
        l = getLabel v
    in
        if isSymbol l then
            makeSymbolTable v <$> foldr getSymbol (return mempty) (rootNode c)
        else
            return mempty
    where
        getSymbol :: LabeledNodeData -> Writer [UniqueError] String -> Writer [UniqueError] String
        getSymbol n w = let
                label = getLabel n
            in do
                id <- w
                if label == textNodeLabel then
                    return $ unwords $ filter (not . null) [id, (getText . getData) n]
                else 
                    return id


getSymbolTable :: (String -> Bool) -> AST LabeledNodeData -> Either [UniqueError] (SymbolTable SymbolAttributes)
getSymbolTable isSymbol ast = let
        (ast', log) = runWriter $ localSymbolTable (astSymbol isSymbol) ast
        ast'' = PostOrderAST ast'
        st = runStateT (mapM_ updateSymTab ast'') mempty
        log' = execWriter st
        symTab = snd . fst. runWriter $ st
    in
        if log == mempty then
            if log' == mempty then
                Right symTab
            else
                Left log'
        else
            Left log



getSymbolPredicate :: UniqueOptions -> String -> Bool
getSymbolPredicate = flip elem . getIdentifierTags


runUniqueTool :: (String -> Maybe String) -> UniqueOptions -> AST LabeledNodeData -> IO ()
runUniqueTool f options = either (printMultipleUniqueErrors h f) printResult . getSymbolTable predicate
    where
        h = stderr
        printResult _ = putStrLn "ok"
        predicate = getSymbolPredicate options
