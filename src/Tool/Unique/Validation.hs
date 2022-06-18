module Tool.Unique.Validation (
    runUniqueTool,
) where


import Control.Monad.State
import Control.Monad.Writer

import System.IO

import qualified Data.Map.Lazy as Map
import Data.Semigroup (Semigroup)

import Language.AST
import Language.SymbolTable
import Tool.Unique.Error (UniqueError (UniqueError), printMultipleErrors)
import Tool.Unique.Options (UniqueOptions)


identifierLabel :: String
identifierLabel = "identifier"

headerLabel :: String
headerLabel = "header"


isDeclareIdentifierLabel :: String -> Bool
isDeclareIdentifierLabel label = label `elem` labels
    where
        labels = [identifierLabel, headerLabel]


updateSymTab :: (LabeledNodeData, SymbolTable) -> StateT SymbolTable (Writer [UniqueError]) ()
updateSymTab (d, tab) = let
        createError :: SymbolTable -> SymbolTable -> String -> UniqueError
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


astSymbol :: AST LabeledNodeData -> Writer [UniqueError] SymbolTable
astSymbol ast@(LabelNode v c) = let
        l = getLabel v
    in
        if isDeclareIdentifierLabel l then
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


validate :: AST LabeledNodeData -> Either [UniqueError] ()
validate ast = let
        (ast', log) = runWriter $ localSymbolTable astSymbol ast
        ast'' = PostOrderAST ast'
        log' = execWriter $ execStateT (mapM_ updateSymTab ast'') mempty
    in
        if log == mempty then
            if log' == mempty then
                Right ()
            else
                Left log'
        else
            Left log



runUniqueTool :: (String -> Maybe s) -> UniqueOptions -> AST LabeledNodeData -> IO ()
runUniqueTool f _ = either (printMultipleErrors h f) printResult . validate
    where
        h = stderr
        printResult _ = putStrLn "ok"
