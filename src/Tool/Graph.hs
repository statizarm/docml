module Tool.Graph (
    renderAST,
    GraphOptions(..),
) where

import Data.Graph.Inductive.Example
import Data.GraphViz
import Data.GraphViz.Printing
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph
import Data.Text.Lazy
import Data.Foldable

import Language.AST
import Control.Monad.State.Lazy


data GraphOptions = GraphOptions {
    output :: String
} deriving(Show, Eq, Ord)


nonExistentNode :: Node
nonExistentNode = 0


rootNodeID :: Node
rootNodeID = 1


getAstEdges :: AST (LNode String) -> [LEdge ()]
getAstEdges (LabelNode (id, _) children) =
    [(id, (fst . getPayload) child, ()) | child <- children] ++ foldMap getAstEdges children


toGraph :: AST LabeledNodeData -> Gr String ()
toGraph ast = mkGraph labeledNodes labeledEdges
    where
        enumerateAST :: AST LabeledNodeData -> AST (LNode String)
        enumerateAST ast = evalState (traverse (\d -> state (\c -> ((c, show d), c + 1))) ast) rootNodeID
        enumeratedAST = enumerateAST ast
        labeledNodes = foldMap (\(id, label) -> [(id, label)]) enumeratedAST
        labeledEdges = getAstEdges enumeratedAST


renderAST :: GraphOptions -> AST LabeledNodeData -> String
renderAST _ = unpack . renderDot . toDot . graphToDot params . toGraph
    where
        params = nonClusteredParams {
            fmtNode = \(_, l) -> [toLabel l]
        }
