module Language.AST (
    AST (..),
    NodeData (..),
    LabeledNodeData,
    labelNode,
    textNode,
) where


import qualified Data.Map.Lazy as Map


type Attributes = Map.Map String String


data NodeData = Attributes {
    getAttributes :: Attributes
} | Text {
    getText :: String
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
} | TextNode {
    getPayload :: a
} | RootNode {
    getChildren :: [AST a]
} deriving(Show, Eq, Ord)


textNode :: String -> AST LabeledNodeData
textNode = TextNode . Labeled "" . Text

labelNode :: String -> [(String, String)] -> [AST LabeledNodeData] -> AST LabeledNodeData
labelNode = (LabelNode .) . (. Attributes . Map.fromList) . Labeled


instance Functor AST where
    fmap f (RootNode children) = RootNode $ (f <$>) <$> children
    fmap f tn@(TextNode attr) = TextNode $ f attr
    fmap f (LabelNode attr children) = LabelNode (f attr) $ (f <$>) <$> children


instance Foldable AST where
    foldMap f (RootNode children) = foldMap (foldMap f) children
    foldMap f (TextNode attr) = f attr
    foldMap f (LabelNode attr children) = f attr <> foldMap (foldMap f) children


instance Traversable AST where
    traverse f (RootNode children) = RootNode <$> sequenceA (traverse f <$> children)
    traverse f (TextNode attr) = TextNode <$> f attr
    traverse f (LabelNode attr children) = LabelNode <$> f attr <*> sequenceA (traverse f <$> children)
