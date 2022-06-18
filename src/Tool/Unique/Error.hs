module Tool.Unique.Error where


import System.IO
import System.Console.ANSI
import Data.Foldable

import Language.SymbolTable


data UniqueError =
    UniqueError { getSymbol :: String
                , getNewSymbolPosition :: SymbolPosition
                , getOldSymbolPosition :: SymbolPosition
    }
    deriving(Show, Eq, Ord)


formatPosition :: SymbolPosition -> String
formatPosition (SymbolPosition (l, c) _ n) = n ++ ":" ++ show l ++ ":" ++ show c


printSingleError :: Handle -> (String -> Maybe s) -> UniqueError -> IO ()
printSingleError h _ (UniqueError name newPos oldPos) = do
    hSetSGR h [SetConsoleIntensity BoldIntensity]
    hPutStr h $ formatPosition newPos ++ " "
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
    hPutStrLn h $ formatPosition oldPos
    hSetSGR h [Reset]



printMultipleErrors :: Foldable t => Handle -> (String -> Maybe s) -> t UniqueError -> IO ()
printMultipleErrors = (traverse_ .) . printSingleError 
