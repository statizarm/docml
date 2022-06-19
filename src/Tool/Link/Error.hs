module Tool.Link.Error where


import System.IO
import System.Console.ANSI

import Language.SymbolTable
import Tool.Link.Symbol
import Tool.Error


data LinkError = UnknownSymbolError String SymbolPosition
               | MultipleIdentifiersError String SymbolPosition [SymbolPosition]
    deriving(Show, Eq, Ord)


printLinkError :: Handle -> (String -> Maybe String) -> LinkError -> IO ()
printLinkError h f (UnknownSymbolError name pos) = do
    hSetSGR h [SetConsoleIntensity BoldIntensity]
    hPutStr h $ show pos ++ " "
    hSetSGR h [Reset]
    hSetSGR h [SetColor Foreground Dull Red]
    hPutStr h "error: "
    hSetSGR h [Reset]
    hPutStr h "unknown "
    hSetSGR h [SetColor Foreground Dull Blue]
    hPutStr h name
    hSetSGR h [Reset]
    hPutStrLn h " in link "
    case f $ getSourceName pos of (Just s) -> printSourceSegment h s pos
                                  Nothing -> return ()

printLinkError h f (MultipleIdentifiersError name linkPos positions) = let
        printFoundPosition pos = do
            hSetSGR h [SetConsoleIntensity BoldIntensity]
            hPutStr h $ show pos ++ " "
            hSetSGR h [Reset]
            hPutStrLn h "found here:"
            case f $ getSourceName pos of (Just s) -> printSourceSegment h s pos
                                          Nothing -> return ()
    in do
        hSetSGR h [SetConsoleIntensity BoldIntensity]
        hPutStr h $ show linkPos ++ " "
        hSetSGR h [Reset]
        hSetSGR h [SetColor Foreground Dull Red]
        hPutStr h "error: "
        hSetSGR h [Reset]
        hPutStr h "multiple symbols for link "
        hSetSGR h [SetColor Foreground Dull Blue]
        hPutStrLn h name
        hSetSGR h [Reset]
        mapM_ printFoundPosition positions
   


printMultipleErrors :: Handle -> (String -> Maybe String) -> [LinkError] -> IO ()
printMultipleErrors = (mapM_ . ) . printLinkError


unknownSymbolError :: String -> SymbolAttributes -> LinkError
unknownSymbolError name = UnknownSymbolError name . getSymbolPosition


multipleIdentifiers :: String -> SymbolAttributes -> Symbol -> LinkError
multipleIdentifiers name attrs = MultipleIdentifiersError name (getSymbolPosition attrs) . map getSymbolPosition
