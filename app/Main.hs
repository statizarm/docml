module Main where

import Options.Applicative
import Data.Semigroup
import Control.Monad
import Text.Parsec.Error

import System.IO

import Tool
import Tool.Graph
import Tool.Unique
import Language.Parser
import Language.AST


data Options = Options {
    getToolCommand :: ToolCommand,
    getSource :: String
} deriving(Show, Eq, Ord)


data ToolCommand = GraphCommand GraphOptions
                 | UniqueCommand UniqueOptions
    deriving(Show, Eq, Ord)

buildGraph :: Parser ToolCommand
buildGraph = GraphCommand <$> (
        GraphOptions
        <$> strOption
        ( long "output"
        <> short 'o'
        <> value "stdout"
        <> showDefault
        )
    )


checkUnique :: Parser ToolCommand
checkUnique = pure (UniqueCommand UniqueOptions)


options :: Parser Options
options = Options
    <$> subparser
        ( command "build-graph" (info (buildGraph <**> helper) (progDesc "build graph"))
        <> command "check-unique" (info checkUnique (progDesc "check uniqueness"))
        )
    <*> argument str
        ( metavar "FILE"
        <> help "Source file"
        <> value "stdin"
        <> showDefault
        )


readFromSource :: String -> IO String
readFromSource "stdin" = getContents
readFromSource fileName = readFile fileName


getTool :: ToolCommand -> Tool
getTool (GraphCommand opts) = GraphTool opts
getTool (UniqueCommand opts) = UniqueTool opts


printResult = putStrLn


runCommand :: Options -> IO()
runCommand options = readFromSource source >>= parse >>= runTool' >>= printResult
    where
        source = getSource options
        toolCommand = getToolCommand options
        tool = getTool toolCommand
        runTool' (Left error) = (return . show) error
        runTool' (Right ast) = let res = runTool tool ast in case res of Left err -> return err
                                                                         Right some -> return some


main :: IO ()
main = runCommand =<< execParser opts
    where
        opts = info (options <**> helper)
            ( fullDesc
            <> progDesc "Check your documtentation"
            )
