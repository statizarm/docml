module Tool.Unique.Error where


import System.IO
import System.Console.ANSI
import Data.Foldable

import Language.SymbolTable
import Text.Printf (hPrintf)


data UniqueError =
    UniqueError { getSymbol :: String
                , getNewSymbolPosition :: SymbolPosition
                , getOldSymbolPosition :: SymbolPosition
    }
    deriving(Show, Eq, Ord)


formatPosition :: SymbolPosition -> String
formatPosition (SymbolPosition (l, c) _ n) = n ++ ":" ++ show l ++ ":" ++ show c


printSourceSegment :: Handle -> String -> SymbolPosition -> IO ()
printSourceSegment h s pos = let
        bl = fst (getBeginPosition pos) - 1
        el = fst (getEndPosition pos)
        lc = el - bl
        ls = take lc . drop bl $ lines s
        ls' = zip [bl .. el] ls
        mns = length . show $ el
        printLine :: (Int, String) -> IO ()
        printLine (n, s) = do
            hPrintf h ("%" ++ show mns ++ "d |\t") $ n + 1
            hSetSGR h [SetColor Foreground Dull Blue]
            hPutStrLn h s
            hSetSGR h [Reset]
    in do
        mapM_ printLine ls'


printSingleError :: Handle -> (String -> Maybe String) -> UniqueError -> IO ()
printSingleError h f (UniqueError name newPos oldPos) = do
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
    case f $ getSourceName oldPos of (Just s) -> printSourceSegment h s oldPos
                                     Nothing -> return ()



printMultipleErrors :: Foldable t => Handle -> (String -> Maybe String) -> t UniqueError -> IO ()
printMultipleErrors = (traverse_ .) . printSingleError 
