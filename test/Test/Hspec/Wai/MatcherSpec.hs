
{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Wai.MatcherSpec (main, spec) where

import           Test.Hspec

import           Network.HTTP.Types
import           Network.Wai.Test

import           Test.Hspec.Wai.Matcher

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "match" $ do
    context "when both status and body do match" $ do
      it "returns Nothing" $ do
        SResponse status200 [] "" `match` code 200
          `shouldBe` Nothing

    context "when status does not match" $ do
      it "returns an error message" $ do
        SResponse status404 [] "" `match` code 200
          `shouldBe` (Just . unlines) [
            "status mismatch:"
          , "  expected: 200"
          , "  but got:  404"
          ]

    context "when body does not match" $ do
      it "returns an error message" $ do
        SResponse status200 [] "foo" `match` body "bar"
          `shouldBe` (Just . unlines) [
            "body mismatch:"
          , "  expected: bar"
          , "  but got:  foo"
          ]

    context "when body contains match" $ do
      it "returns Nothing" $ do
        SResponse status200 [] "foo bar" `match` body "bar"
          `shouldBe` Nothing

      context "when one body contains unsafe characters" $ do
        it "uses show for that body in the error message" $ do
          SResponse status200 [] "foo\nfoo" `match` body "bar"
            `shouldBe` (Just . unlines) [
              "body mismatch:"
            , "  expected: bar"
            , "  but got:  \"foo\\nfoo\""
            ]

    context "when both status and body do not match" $ do
      it "combines error messages" $ do
        SResponse status404 [] "foo" `match` (code 200) { matchBody = body' "bar" }
          `shouldBe` (Just . unlines) [
            "status mismatch:"
          , "  expected: 200"
          , "  but got:  404"
          , "body mismatch:"
          , "  expected: bar"
          , "  but got:  foo"
          ]      
  
    context "when matching css selectors" $ do
       
      it "can find matches for a selector" $ do
        selectorMatches ".heading"  "<h1 class=\"heading\">Test</h1>"
          `shouldBe` ["Test"]
      
      it "can find matches for a selector" $ do
        selectorMatches ".wrong"  "<h1 class=\"heading\">Test</h1>"
          `shouldBe` []

      it "can match selectors" $ do
        (hasSelector' ".heading" "<h1 class=\"heading\">Test</h1>")
        `shouldBe` True

      it "can match selectors" $ do
        (hasSelector' ".wrong" "<h1 class=\"heading\">Test</h1>")
        `shouldBe` False
      
      it "can match text in selectors" $ do
        (hasSelector ".heading" "Test" "<h1 class=\"heading\">Test</h1>")
        `shouldBe` True
      
      it "matches with correct selector" $ do
        SResponse status200 [] "<h1 class=\"heading\">Test</h1>"
          `match` css ".heading" "Test"
          `shouldBe` Nothing
          
      it "doesn't match with wrong selector" $ do
        SResponse status200 [] "<h1 class=\"wrong\">Test</h1>"
          `match` css ".heading" "Test"
          `shouldBe` (Just . unlines) [
            "selector mismatch:"
          , "  expected: \"Test\" in .heading"
          , "  but got:  Selector not found." ]
{--
    context "when matching headers" $ do
      context "when header is missing" $ do
        it "returns an error message" $ do
          SResponse status200 [] "" `match` code 200 {matchHeaders = ["Content-Type" <:> "application/json"]}
            `shouldBe` (Just . unlines) [
              "missing header:"
            , "  Content-Type: application/json"
            , "the actual headers were:"
            ]

      context "when multiple headers are missing" $ do
        it  "combines error messages" $ do
          let expectedHeaders = ["Content-Type" <:> "application/json", "Content-Encoding" <:> "chunked"]
          SResponse status200 [(hContentLength, "23")] "" `match` 200 {matchHeaders = expectedHeaders}
            `shouldBe` (Just . unlines) [
              "missing header:"
            , "  Content-Type: application/json"
            , "missing header:"
            , "  Content-Encoding: chunked"
            , "the actual headers were:"
            , "  Content-Length: 23"
            ]
--}
