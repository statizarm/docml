module Tool.Unique.Symbol where


import qualified Data.Map as Map

import Language.AST
import Language.SymbolTable


makeSymbolTable :: LabeledNodeData -> String -> SymbolTable SymbolAttributes
makeSymbolTable v n = Map.fromList [(n, symbolAttributes v)]
