module Tool.Print where

import Language.AST

rawPrint :: AST LabeledNodeData -> String
rawPrint = show
