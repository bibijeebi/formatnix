module Main where

import qualified Data.Text.IO as TIO
import NixParser (formatNix, parseNixExpr)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  contents <- TIO.getContents
  case parseNixExpr contents of
    Left err -> putStrLn $ errorBundlePretty err
    Right expr -> TIO.putStr $ formatNix expr
