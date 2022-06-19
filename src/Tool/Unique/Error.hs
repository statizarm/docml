module Tool.Unique.Error where


import System.IO
import System.Console.ANSI
import Data.Foldable

import Language.SymbolTable

import Tool.Error
import Text.Printf (hPrintf)


data UniqueError =
    UniqueError { getSymbol :: String
                , getNewSymbolPosition :: SymbolPosition
                , getOldSymbolPosition :: SymbolPosition
    }
    deriving(Show, Eq, Ord)


printUniqueError :: Handle -> (String -> Maybe String) -> UniqueError -> IO ()
printUniqueError h f (UniqueError name newPos oldPos) = do
    hSetSGR h [SetConsoleIntensity BoldIntensity]
    hPutStr h $ show newPos ++ " "
    hSetSGR h [Reset]
    hSetSGR h [SetColor Foreground Dull Red]
    hPutStr h "error: "
    hSetSGR h [Reset]
    hPutStr h "found previous declaration for "
    hSetSGR h [SetColor Foreground Dull Blue]
    hPutStr h name
    hSetSGR h [Reset]
    hPutStrLn h " at"
    hSetSGR h [SetConsoleIntensity BoldIntensity]
    hPrint h oldPos
    hSetSGR h [Reset]
    case f $ getSourceName oldPos of (Just s) -> printSourceSegment h s oldPos
                                     Nothing -> return ()


printMultipleUniqueErrors :: Foldable t => Handle -> (String -> Maybe String) -> t UniqueError -> IO ()
printMultipleUniqueErrors = (traverse_ .) . printUniqueError 
