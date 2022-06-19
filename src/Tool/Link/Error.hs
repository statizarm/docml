module Tool.Link.Error where


import System.IO

import Tool.Link.Symbol


data LinkError = UnknownSymbolError
               | MultipleIdentifiersError
    deriving(Show, Eq, Ord)


printLinkError :: Handle -> LinkError -> IO ()
printLinkError h e = do
    hPrint h e


printMultipleErrors :: Handle -> [LinkError] -> IO ()
printMultipleErrors = mapM_ . printLinkError


unknownSymbolError :: String -> Symbol -> LinkError
unknownSymbolError _ _ = UnknownSymbolError


multipleIdentifiers :: String -> Symbol -> LinkError
multipleIdentifiers _ _ = MultipleIdentifiersError
