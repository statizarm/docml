module Language.AST (
    AST (..),
    NodeData (..),
    LabeledNodeData,
    rootNodeLabel,
    textNodeLabel,
    rootNode,
    labelNode,
    textNode,
) where


import qualified Data.Map.Lazy as Map
import Prelude
import Data.Traversable (foldMapDefault)


type Attributes = Map.Map String String


data NodeData = Attributes {
    getAttributes :: Attributes
} | Text {
    getText :: String
} | Empty {
} deriving(Show, Eq, Ord)


data Labeled a = Labeled {
    getLabel :: String,
    getData :: a
} deriving(Show, Eq, Ord)


instance Functor Labeled where
    fmap f (Labeled l d) = Labeled l $ f d


type LabeledNodeData = Labeled NodeData


data AST a = LabelNode {
    getPayload :: a,
    getChildren :: [AST a]
} deriving(Show, Eq, Ord)


rootNodeLabel = "_RootNode_"
textNodeLabel = "_TextNode_"

rootNode :: [AST LabeledNodeData] -> AST LabeledNodeData
rootNode = LabelNode (Labeled rootNodeLabel Empty)

textNode :: String -> AST LabeledNodeData
textNode text = LabelNode (Labeled textNodeLabel (Text text)) []

labelNode :: String -> [(String, String)] -> [AST LabeledNodeData] -> AST LabeledNodeData
labelNode = (LabelNode .) . (. Attributes . Map.fromList) . Labeled


instance Functor AST where
    fmap f (LabelNode attr children) = LabelNode (f attr) $ (f <$>) <$> children


instance Foldable AST where
    foldMap f (LabelNode attr children) = f attr <> foldMap (foldMap f) children


instance Traversable AST where
    traverse f (LabelNode attr children) = LabelNode <$> f attr <*> sequenceA (traverse f <$> children)
