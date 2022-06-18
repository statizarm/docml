module Main where


import Options.Applicative hiding (reader, ParseError)
import Data.Semigroup
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Reader
import Text.Parsec.Error

import System.IO

import Tool.Graph.Options
import Tool.Graph.Dot
import Tool.Unique.Validation
import Tool.Unique.Options

import Language.Parser
import Language.AST


data Options = Options {
    getToolCommand :: ToolCommand,
    getSources :: [String]
} deriving(Show, Eq, Ord)


data ToolCommand = GraphCommand GraphOptions
                 | UniqueCommand UniqueOptions
    deriving(Show, Eq, Ord)


buildGraph :: Parser ToolCommand
buildGraph = GraphCommand <$> graphOptionsParser


checkUnique :: Parser ToolCommand
checkUnique = UniqueCommand <$> uniqueOptionsParser


options :: Parser Options
options = Options
    <$> subparser
        ( command "build-graph" (info buildGraph (progDesc "build graph"))
        <> command "check-unique" (info checkUnique (progDesc "check uniqueness"))
        )
    <*> many (
        argument str
            ( metavar "FILE"
            <> help "Source file"
            )
        )


readFromSource :: String -> IO String
readFromSource "stdin" = getContents
readFromSource fileName = readFile fileName


runSpecificTool :: ToolCommand -> AST LabeledNodeData -> Map.Map String String -> IO()
runSpecificTool (GraphCommand options) ast _ = runGraphTool options ast
runSpecificTool (UniqueCommand options) ast sourcesMap = runUniqueTool (sourcesMap Map.!?) options ast


runCommand :: Options -> IO()
runCommand options = do
        sources <- traverse (\x -> (,) x <$> readFromSource x) $ getSources options
        sourcesMap <- return . Map.fromList $ sources
        ast <- parse sources
        runTool' ast sourcesMap
    where
        toolCommand = getToolCommand options
        runTool' :: Either ParseError (AST LabeledNodeData) -> Map.Map String String -> IO ()
        runTool' (Left error) _ = hPrint stderr error
        runTool' (Right ast) sm = runSpecificTool toolCommand ast sm


main :: IO ()
main = runCommand =<< execParser opts
    where
        opts = info (options <**> helper)
            ( fullDesc
            <> progDesc "Check your documtentation"
            )
