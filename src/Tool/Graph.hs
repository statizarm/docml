module Tool.Graph (
    renderAST,
    GraphOptions(..),
) where

import Data.Graph.Inductive.Example
import Data.GraphViz
import Data.GraphViz.Printing
import Data.Graph.Inductive.PatriciaTree
import Data.Text.Lazy

import Language.AST


data GraphOptions = GraphOptions {
    output :: String
} deriving(Show, Eq, Ord)


toGraph :: AST LabeledNodeData -> Gr Char ()
toGraph ast = clr479


renderAST :: GraphOptions -> AST LabeledNodeData -> String
renderAST _ = unpack . renderDot . toDot . graphToDot params . toGraph
    where
        params = nonClusteredParams
