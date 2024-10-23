module Main where

import qualified NixParserSpec
import Test.Hspec

main :: IO ()
main = hspec NixParserSpec.spec
