module Language.SymbolTable where


import qualified Data.Map as Map
import Language.AST


type NLine = Int
type NColumn = Int


data SymbolPosition = SymbolPosition {
    getBeginPosition :: (NLine, NColumn),
    getEndPosition :: (NLine, NColumn),
    getSourceName :: String
} deriving(Show, Eq, Ord)


data SymbolAttributes = SymbolAttributes {
    getSymbolLabel :: String,
    getSymbolPosition :: SymbolPosition
} deriving(Show, Eq, Ord)


type SymbolTable = Map.Map String SymbolAttributes


localSymbolTable :: Monad m => (AST LabeledNodeData -> m SymbolTable) -> AST LabeledNodeData -> m (AST (LabeledNodeData, SymbolTable))
localSymbolTable f ast@(LabelNode nodeData children) = LabelNode . (,) nodeData <$> f ast <*> foo
    where foo = traverse (localSymbolTable f) children


makeSymbolTable :: LabeledNodeData -> String -> SymbolTable
makeSymbolTable v n = Map.fromList [(n, symbolAttributes v)]


symbolAttributes :: LabeledNodeData -> SymbolAttributes
symbolAttributes v = let
        label = getLabel v
    in SymbolAttributes label $ symbolPosition . getData $ v


symbolPosition :: NodeData -> SymbolPosition
symbolPosition Empty = emptySymbolPosition
symbolPosition nodeData = let
        (MetaAttributes sn bl bc el ec) = getMeta nodeData
    in SymbolPosition (bl, bc) (el, ec) sn


emptySymbolPosition :: SymbolPosition
emptySymbolPosition = SymbolPosition (0, 0) (0, 0) ""
