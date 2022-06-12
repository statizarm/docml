{-# LANGUAGE FlexibleInstances #-}
module Tool (
    Tool(..),
    runTool,
) where


import Options.Applicative
import Language.AST (AST, LabeledNodeData)

import Tool.Graph (GraphOptions, renderAST)
import Tool.Unique (UniqueOptions)


data Tool = GraphTool {getGraphOptions :: GraphOptions}
          | UniqueTool {getUniqueOptions :: UniqueOptions}
    deriving(Show, Eq, Ord)


runTool :: Tool -> AST LabeledNodeData -> Either String String
runTool (GraphTool options) = Right . renderAST options
runTool (UniqueTool options) = undefined
