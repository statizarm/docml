module Tool.Graph.Dot (
    runGraphTool,
    GraphOptions(..),
) where

import qualified Data.Map as Map
import Data.Graph.Inductive.Example
import Data.GraphViz
import Data.GraphViz.Printing
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph
import Data.Text.Lazy hiding (length, take, drop)
import Data.Foldable
import Control.Monad.State.Lazy

import Language.AST
import Tool.Graph.Options
import System.IO (hPutStrLn, Handle, stdout, openFile, IOMode (..), hClose)

nonExistentNode :: Node
nonExistentNode = 0


rootNodeID :: Node
rootNodeID = 1


getAstEdges :: AST (LNode LabeledNodeData) -> [LEdge ()]
getAstEdges (LabelNode (id, _) children) =
    [(id, (fst . getPayload) child, ()) | child <- children] ++ foldMap getAstEdges children


toGraph :: AST LabeledNodeData -> Gr LabeledNodeData ()
toGraph ast = mkGraph labeledNodes labeledEdges
    where
        enumerateAST :: AST LabeledNodeData -> AST (LNode LabeledNodeData)
        enumerateAST ast = evalState (traverse (\d -> state (\c -> ((c, d), c + 1))) ast) rootNodeID
        enumeratedAST = enumerateAST ast
        labeledNodes = foldMap (\(id, label) -> [(id, label)]) enumeratedAST
        labeledEdges = getAstEdges enumeratedAST


renderAST :: GraphOptions -> AST LabeledNodeData -> String
renderAST _ = unpack . renderDot . toDot . graphToDot params . toGraph
    where
        collapseText lim t = if len > lim + length inf then pref ++ inf ++ suf else t
            where
                inf = " ... "
                len = length t
                half = lim `div` 2
                pref = take half t
                suf = drop (len - half) t

        fmtNodeData (Text _ t) = "Text = \"" ++ collapseText 16 t ++ "\""
        fmtNodeData (Attributes _ attrs) = Map.foldrWithKey joinAttrs "" attrs
            where
                joinAttrs = \k v pref -> pref ++ "." ++ k ++ " = " ++ v ++ "\n"
        fmtNodeData Empty = ""

        fmtLabeledNodeData d = 
            "label = " ++ getLabel d ++ "\n" ++ (fmtNodeData . getData) d
        params = nonClusteredParams {
            fmtNode = \(_, l) -> [toLabel $ fmtLabeledNodeData l]
        }


getHandle :: String -> IO Handle
getHandle "stdout" = return stdout
getHandle fileName = openFile fileName WriteMode


runGraphTool :: GraphOptions -> AST LabeledNodeData -> IO ()
runGraphTool options ast = let
        handle = getHandle $ getOutput options
        dot = renderAST options ast
    in do
        h <- handle
        hPutStrLn h dot
        hClose h
