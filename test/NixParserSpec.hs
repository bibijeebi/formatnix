{-# LANGUAGE OverloadedStrings #-}

module NixParserSpec (spec) where

import NixParser
import Test.Hspec

spec :: Spec
spec = do
  describe "NixParser" $ do
    describe "basic parsing" $ do
      it "parses simple strings" $
        case parseNixExpr "\"hello\"" of
          Right (NixString "hello") -> pure ()
          other -> expectationFailure $ "unexpected result: " ++ show other

      it "parses simple lists" $
        case parseNixExpr "[ \"a\" \"b\" ]" of
          Right (NixList [NixString "a", NixString "b"]) -> pure ()
          other -> expectationFailure $ "unexpected result: " ++ show other

      it "parses simple attribute sets" $
        case parseNixExpr "{ x = \"1\"; }" of
          Right (NixAttrSet [("x", NixString "1")]) -> pure ()
          other -> expectationFailure $ "unexpected result: " ++ show other

      it "parses boolean values" $ do
        case parseNixExpr "true" of
          Right (NixBool True) -> pure ()
          other -> expectationFailure $ "unexpected result for true: " ++ show other
        case parseNixExpr "false" of
          Right (NixBool False) -> pure ()
          other -> expectationFailure $ "unexpected result for false: " ++ show other

      it "parses null" $
        case parseNixExpr "null" of
          Right NixNull -> pure ()
          other -> expectationFailure $ "unexpected result: " ++ show other

      it "parses paths" $
        case parseNixExpr "./some/path" of
          Right (NixPath "./some/path") -> pure ()
          other -> expectationFailure $ "unexpected result: " ++ show other

      it "parses let expressions" $
        case parseNixExpr "let x = \"1\"; in x" of
          Right (NixLet [("x", NixString "1")] (NixIdent "x")) -> pure ()
          other -> expectationFailure $ "unexpected result: " ++ show other

      it "parses with expressions" $
        case parseNixExpr "with { x = \"1\"; }; x" of
          Right (NixWith (NixAttrSet [("x", NixString "1")]) (NixIdent "x")) -> pure ()
          other -> expectationFailure $ "unexpected result: " ++ show other

    describe "error handling" $ do
      it "fails on invalid syntax" $ do
        case parseNixExpr "{ invalid" of
          Left _ -> pure () -- Expected to fail
          Right expr -> expectationFailure $ "Should fail but got: " ++ show expr

      it "fails on unclosed string" $ do
        case parseNixExpr "\"unclosed" of
          Left _ -> pure () -- Expected to fail
          Right expr -> expectationFailure $ "Should fail but got: " ++ show expr

    describe "formatting" $ do
      it "formats strings correctly" $
        formatNix (NixString "hello") `shouldBe` "\"hello\""

      it "formats lists correctly" $ do
        let input = NixList [NixString "a", NixString "b"]
        let expected = "[\n  \"a\"\n  \"b\"\n]"
        formatNix input `shouldBe` expected

      it "formats attribute sets correctly" $ do
        let input = NixAttrSet [("x", NixString "1"), ("y", NixString "2")]
        let expected = "{\n  x = \"1\";\n  y = \"2\";\n}"
        formatNix input `shouldBe` expected

      it "formats boolean values correctly" $ do
        formatNix (NixBool True) `shouldBe` "true"
        formatNix (NixBool False) `shouldBe` "false"

      it "formats null correctly" $
        formatNix NixNull `shouldBe` "null"

      it "formats let expressions correctly" $ do
        let input = NixLet [("x", NixString "1")] (NixIdent "x")
        let expected = "let\n  x = \"1\";\nin\nx"
        formatNix input `shouldBe` expected

      it "formats with expressions correctly" $ do
        let input = NixWith (NixAttrSet [("x", NixString "1")]) (NixIdent "x")
        let expected = "with {\n  x = \"1\";\n};\nx"
        formatNix input `shouldBe` expected