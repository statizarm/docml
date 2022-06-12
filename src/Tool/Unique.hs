module Tool.Unique (
    UniqueOptions(..),
) where


import Control.Monad.State

import Language.AST

import qualified Data.Map.Lazy as Map
import Data.Semigroup (Semigroup)


data UniqueOptions = UniqueOptions
    deriving(Show, Eq, Ord)


newtype SymTab = SymTab{getTab :: Map.Map String String}


instance Semigroup SymTab where
    (SymTab l) <> (SymTab r) = SymTab $ Map.union l r


instance Monoid SymTab where
    mempty = SymTab Map.empty
    mappend = (<>)


getLocalSymTab :: AST LabeledNodeData -> AST SymTab
getLocalSymTab ast = (evalState . traverse calcLocalSymTab) ast mempty
    where
        calcLocalSymTab :: LabeledNodeData -> State SymTab SymTab
        calcLocalSymTab = undefined


validate :: AST LabeledNodeData -> Either String (AST ())
validate ast = sequenceA $ (evalState . traverse validate' . getLocalSymTab) ast mempty
    where
        validate' :: SymTab -> State SymTab (Either String ())
        validate' = undefined
