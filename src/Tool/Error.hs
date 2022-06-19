module Tool.Error where


import System.IO
import System.Console.ANSI

import Text.Printf (hPrintf)

import Language.SymbolTable


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

