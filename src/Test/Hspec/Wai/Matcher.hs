{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hspec.Wai.Matcher (
  ResponseMatcher(..)
, code
, body
, headers
, code'
, body'
, headers'
, match
) where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString         (ByteString, isInfixOf)
import qualified Data.ByteString.Lazy    as LB
import qualified Data.ByteString.Lazy    as LB
import           Data.Maybe
import           Data.Maybe
import           Data.Monoid
import Data.CaseInsensitive (mk)
import           Data.String
import           Data.Text               (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Network.HTTP.Types
import           Network.Wai.Test
import           Test.Hspec.Wai.Util

data ResponseMatcher = ResponseMatcher {
  matchStatus  :: (Int -> Bool, Text)
, matchHeaders :: ([Header] -> Bool, Text)
, matchBody    :: (LB.ByteString -> Bool, Text)
}

defaultMatcher :: ResponseMatcher
defaultMatcher =
  ResponseMatcher {
    matchStatus = (const True, "")
  , matchHeaders = (const True, "")
  , matchBody = (const True, "") }

code :: Int -> ResponseMatcher
code n = defaultMatcher { matchStatus = ((== 200), T.pack $ show n) }

body :: Text -> ResponseMatcher
body t = defaultMatcher
  { matchBody =
     (\x -> isInfixOf (encodeUtf8 t) (LB.toStrict x), t) }

headers :: [(Text, Text)] -> ResponseMatcher
headers xs = defaultMatcher
  { matchHeaders =
      ((\hs -> and $ map (`elem` hs) ts), T.pack $ show xs) }
  where ts = map (\(x,y) -> (mk $ toBS x, toBS y)) xs

code' :: Int -> (Int -> Bool, Text)
code' n = ((== 200), T.pack $ show n)

body' :: Text -> (LB.ByteString -> Bool, Text)
body' t = (\x -> isInfixOf (encodeUtf8 t) (LB.toStrict x), t)

headers' :: [(Text, Text)] -> ([Header] -> Bool, Text)
headers' xs = ((\hs -> and $ map (`elem` hs) ts), T.pack $ show xs)
  where ts = map (\(x,y) -> (mk $ toBS x, toBS y)) xs

toBS :: Text -> ByteString
toBS t = encodeUtf8 t

-- data MatchHeader = MatchHeader ([Header] -> Maybe String)

-- instance IsString ResponseMatcher where
--   fromString s = ResponseMatcher 200 [] (Just . encodeUtf8 . fromString $ s)

-- instance Num ResponseMatcher where
--   fromInteger n = ResponseMatcher (fromInteger n) [] Nothing
--   (+) =    error "ResponseMatcher does not support (+)"
--   (-) =    error "ResponseMatcher does not support (-)"
--   (*) =    error "ResponseMatcher does not support (*)"
--   abs =    error "ResponseMatcher does not support `abs`"
--   signum = error "ResponseMatcher does not support `signum`"

match :: SResponse -> ResponseMatcher -> Maybe String
match (SResponse (Status status _) headers body) ResponseMatcher{..} = mconcat [
    actualExpected "status mismatch:" (show status) (toStr' (snd matchStatus)) <$ guard (not $ fst matchStatus status)
  , actualExpected "headers mismatch:" (show headers) (toStr' (snd matchHeaders)) <$  guard (not $ fst matchHeaders headers)
  , actualExpected "body mismatch:" (toStr body) (toStr' (snd matchBody)) <$  guard (not $ fst matchBody body)
  ]
  where
    -- matchBody_ (toStrict -> actual) (toStrict -> expected) = actualExpected "body mismatch:" actual_ expected_ <$ guard (actual `doesNotContain` expected)
    --   where
    --     (actual_, expected_) = case (safeToString actual, safeToString expected) of
    --       (Just x, Just y) -> (x, y)
    --       _ -> (show actual, show expected)
    toStr :: LB.ByteString -> String
    toStr s = fromMaybe (show s) (safeToString (LB.toStrict s))
    toStr' :: Text -> String
    toStr' t = fromMaybe (show t) (safeToString (encodeUtf8 t))
    actualExpected :: String -> String -> String -> String
    actualExpected message actual expected = unlines [
        message
      , "  expected: " ++ expected
      , "  but got:  " ++ actual
      ]

doesNotContain :: ByteString -> ByteString -> Bool
doesNotContain a b = not (b `isInfixOf` a)
