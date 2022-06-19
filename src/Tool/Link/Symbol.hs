module Tool.Link.Symbol where


import Language.AST (LabeledNodeData)
import Language.SymbolTable (SymbolAttributes, SymbolTable, symbolAttributes)

import qualified Data.Map as Map


type Symbol = [SymbolAttributes]


makeSymbolTable :: LabeledNodeData -> String -> SymbolTable Symbol
makeSymbolTable v n = Map.fromList [(n, [symbolAttributes v])]


addSymbol :: SymbolTable Symbol -> String -> SymbolAttributes -> SymbolTable Symbol
addSymbol tab s a = Map.insertWith (++) s [a] tab
