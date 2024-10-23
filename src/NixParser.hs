{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module NixParser
  ( -- Types
    Parser,
    NixExpr (..),
    -- Parsing
    nixExpr,
    parseNixExpr,
    -- Formatting
    formatNix,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser type alias for better readability
type Parser = Parsec Void Text

-- | Parser for paths starting with ./
nixPath :: Parser NixExpr
nixPath =
  NixPath . T.pack
    <$> lexeme
      ( char '.'
          >> ( try (char '/' >> many (oneOf allowedChars))
                 <|> pure ""
             )
      )
  where
    allowedChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_.+/"

-- | Abstract Syntax Tree for Nix expressions
data NixExpr
  = NixAttrSet [(Text, NixExpr)]
  | NixList [NixExpr]
  | NixString Text
  | NixPath Text
  | NixIdent Text
  | NixBool Bool
  | NixNull
  | NixLet [(Text, NixExpr)] NixExpr
  | NixWith NixExpr NixExpr
  deriving (Show, Eq)

-- | Space consumer that handles comments and whitespace
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "#")
    (L.skipBlockComment "/*" "*/")

-- | Wrapper for lexemes
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol parser with automatic space consumption
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parser for string literals
stringLiteral :: Parser NixExpr
stringLiteral =
  NixString . T.pack
    <$> (char '"' *> manyTill L.charLiteral (char '"'))

-- | Parser for identifiers
identifier :: Parser Text
identifier = lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> oneOf ['_', '-'])
  return $ T.pack (first : rest)

-- | Parser for boolean values
nixBool :: Parser NixExpr
nixBool =
  lexeme $
    (symbol "true" >> return (NixBool True))
      <|> (symbol "false" >> return (NixBool False))

-- | Parser for null value
nixNull :: Parser NixExpr
nixNull = NixNull <$ symbol "null"

-- | Parser for lists
nixList :: Parser NixExpr
nixList =
  NixList
    <$> between
      (symbol "[")
      (symbol "]")
      (many nixExpr)

-- | Parser for attribute sets
nixAttrSet :: Parser NixExpr
nixAttrSet =
  NixAttrSet
    <$> between
      (symbol "{")
      (symbol "}")
      (many ((,) <$> identifier <* symbol "=" <*> nixExpr <* symbol ";"))

-- | Parser for let expressions
nixLet :: Parser NixExpr
nixLet =
  NixLet
    <$> (symbol "let" *> many binding)
    <*> (symbol "in" *> nixExpr)
  where
    binding = (,) <$> identifier <* symbol "=" <*> nixExpr <* symbol ";"

-- | Parser for with expressions
nixWith :: Parser NixExpr
nixWith =
  NixWith
    <$> (symbol "with" *> nixExpr <* symbol ";")
    <*> nixExpr

-- | Main expression parser
nixExpr :: Parser NixExpr
nixExpr =
  lexeme $
    choice
      [ nixAttrSet,
        nixList,
        stringLiteral,
        nixPath,
        nixBool,
        nixNull,
        nixLet,
        nixWith,
        NixIdent <$> identifier
      ]

-- | Helper function for parsing Nix expressions
parseNixExpr :: Text -> Either (ParseErrorBundle Text Void) NixExpr
parseNixExpr = parse nixExpr "input"

-- | Formats a Nix expression with proper indentation
formatNix :: NixExpr -> Text
formatNix = formatWithIndent 0

-- | Helper function for indented formatting
formatWithIndent :: Int -> NixExpr -> Text
formatWithIndent indent = \case
  NixString s -> "\"" <> s <> "\""
  NixPath p -> p
  NixIdent i -> i
  NixBool True -> "true"
  NixBool False -> "false"
  NixNull -> "null"
  NixList exprs -> formatList indent exprs
  NixAttrSet attrs -> formatAttrSet indent attrs
  NixLet bindings expr -> formatLet indent bindings expr
  NixWith expr body -> formatWith indent expr body

-- | Helper function for formatting lists
formatList :: Int -> [NixExpr] -> Text
formatList indent exprs =
  "[\n"
    <> mconcat (map formatItem exprs)
    <> indentText indent
    <> "]"
  where
    formatItem expr =
      indentText (indent + 2)
        <> formatWithIndent (indent + 2) expr
        <> "\n"

-- | Helper function for formatting attribute sets
formatAttrSet :: Int -> [(Text, NixExpr)] -> Text
formatAttrSet indent attrs =
  "{\n"
    <> mconcat (map formatAttr attrs)
    <> indentText indent
    <> "}"
  where
    formatAttr (key, val) =
      indentText (indent + 2)
        <> key
        <> " = "
        <> formatWithIndent (indent + 2) val
        <> ";\n"

-- | Helper function for formatting let expressions
formatLet :: Int -> [(Text, NixExpr)] -> NixExpr -> Text
formatLet indent bindings expr =
  "let\n"
    <> mconcat (map formatBinding bindings)
    <> "in\n"
    <> formatWithIndent indent expr
  where
    formatBinding (name, val) =
      indentText (indent + 2)
        <> name
        <> " = "
        <> formatWithIndent (indent + 2) val
        <> ";\n"

-- | Helper function for formatting with expressions
formatWith :: Int -> NixExpr -> NixExpr -> Text
formatWith indent expr body =
  "with "
    <> formatWithIndent indent expr
    <> ";\n"
    <> formatWithIndent indent body

-- | Helper function for creating indentation
indentText :: Int -> Text
indentText n = T.replicate n " "
