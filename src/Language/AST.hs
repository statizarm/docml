module Language.AST (
    AST (..),
    NodeData (..),
    Labeled (..),
    PostOrderAST(..),
    MetaAttributes(..),
    LabeledNodeData,
    rootNodeLabel,
    textNodeLabel,
    rootNode,
    labelNode,
    textNode,
    nestedTraverse,
) where


import qualified Data.Map.Lazy as Map
import Prelude
import qualified Text.Parsec as Parsec
import Data.Traversable (foldMapDefault)


type Attributes = Map.Map String String


data MetaAttributes = MetaAttributes {
    getSourceName  :: String,
    getBeginLine   :: Int,
    getBeginColumn :: Int,
    getEndLine     :: Int,
    getEndColumn   :: Int
} deriving(Show, Eq, Ord)


data NodeData = Attributes {
    getMeta :: MetaAttributes,
    getAttributes :: Attributes
} | Text {
    getMeta :: MetaAttributes,
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


fillMetaAttributes :: Parsec.SourcePos -> Parsec.SourcePos -> MetaAttributes
fillMetaAttributes begin end =
    MetaAttributes { getSourceName = Parsec.sourceName begin
                   , getBeginLine = Parsec.sourceLine begin
                   , getBeginColumn = Parsec.sourceColumn begin
                   , getEndLine = Parsec.sourceLine end
                   , getEndColumn = Parsec.sourceColumn end
    }

rootNode :: [AST LabeledNodeData] -> AST LabeledNodeData
rootNode = LabelNode (Labeled rootNodeLabel Empty)

textNode :: Parsec.SourcePos -> String -> Parsec.SourcePos -> AST LabeledNodeData
textNode beginPos text endPos = LabelNode (Labeled textNodeLabel text') []
    where
        meta = fillMetaAttributes beginPos endPos
        text' = Text meta . unwords . words $ text

labelNode :: Parsec.SourcePos -> String -> [(String, String)] -> [AST LabeledNodeData] -> Parsec.SourcePos -> AST LabeledNodeData
labelNode beginPos label attrs children endPos = let
        meta = fillMetaAttributes beginPos endPos
        attributes = Attributes meta . Map.fromList $ attrs
    in LabelNode (Labeled label attributes) children


nestedTraverse :: Monad m => (AST a -> m b) -> AST a -> m (AST b)
nestedTraverse f ast@(LabelNode _ c) = LabelNode <$> f ast <*> traverse (nestedTraverse f) c


instance Functor AST where
    fmap f (LabelNode attr children) = LabelNode (f attr) $ (f <$>) <$> children


instance Foldable AST where
    foldMap f (LabelNode attr children) = f attr <> foldMap (foldMap f) children


instance Traversable AST where
    traverse f (LabelNode attr children) = LabelNode <$> f attr <*> sequenceA (traverse f <$> children)


newtype PostOrderAST a = PostOrderAST {getAST :: AST a}


instance Functor PostOrderAST where
    fmap f (PostOrderAST ast) = PostOrderAST $ f <$> ast


instance Foldable PostOrderAST where
    foldMap = foldMapDefault


instance Traversable PostOrderAST where
    traverse f (PostOrderAST (LabelNode attr children)) = PostOrderAST <$>
        (flip LabelNode <$>
            ((getAST <$>) <$> traverse (traverse f) (PostOrderAST <$> children))
            <*> f attr
        )
