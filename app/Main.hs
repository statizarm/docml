module Main where

import Language.Parser
import Tool.Graph

import System.IO

main :: IO ()
main = getContents >>= parse >>= print
