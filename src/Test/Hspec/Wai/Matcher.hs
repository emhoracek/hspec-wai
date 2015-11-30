{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Test.Hspec.Wai.Matcher (
  ResponseMatcher(..)
, code
, body
, css
, headers
, code'
, body'
, headers'
, hasSelector
, hasSelector'
, selectorMatches
, match
) where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString      (ByteString, isInfixOf, unpack)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy as LB
import           Data.CaseInsensitive (mk)
import           Data.Maybe
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.Encoding   (encodeUtf8, decodeUtf8)
import           Network.HTTP.Types
import           Network.Wai.Test
import           Test.Hspec.Wai.Util
import qualified Text.HandsomeSoup    as HS
import qualified Text.XML.HXT.Core    as HXT

data ResponseMatcher = ResponseMatcher {
  matchStatus  :: (Int -> Bool, Text)
, matchHeaders :: ([Header] -> Bool, Text)
, matchBody    :: (LB.ByteString -> Bool, Text)
, matchCss     :: (LB.ByteString -> Bool, Text)
, selector     :: Text
}

defaultMatcher :: ResponseMatcher
defaultMatcher =
  ResponseMatcher {
    matchStatus = (const True, "")
  , matchHeaders = (const True, "")
  , matchBody = (const True, "")
  , matchCss = (const True, "")
  , selector = ""}

code :: Int -> ResponseMatcher
code n = defaultMatcher { matchStatus = ((== 200), T.pack $ show n) }

body :: Text -> ResponseMatcher
body t = defaultMatcher
  { matchBody =
     (\x -> isInfixOf (encodeUtf8 t) (LB.toStrict x), t) }

css :: Text -> Text -> ResponseMatcher
css selector t = (defaultMatcher
  { matchCss =
    (hasSelector selector t, "\"" <> t <> "\" in " <> selector) }) {selector = selector}

{--
haveSelector' :: Text -> TestResponse -> Bool
haveSelector' selector (Html _ body) =
  case HXT.runLA (HXT.hread HXT.>>> HS.css (T.unpack selector)) (T.unpack body)  of
    [] -> False
    _ -> True
haveSelector' _ _ = False--}

hasSelector :: Text -> Text -> LB.ByteString -> Bool
hasSelector selector text body'' =
  let stringBody = toS body''
      selectorText = HXT.runLA (HXT.hread HXT.>>> HS.css (T.unpack selector) HXT./> HXT.getText) stringBody in
      text `elem` map T.pack selectorText

selectorMatches :: Text -> String -> [String]
selectorMatches selector b= HXT.runLA (HXT.hread HXT.>>> HS.css (T.unpack selector) HXT./> HXT.getText) b

toS :: LB.ByteString -> String
toS b = T.unpack $ decodeUtf8 $ LB.toStrict b

hasSelector' :: Text -> LB.ByteString -> Bool
hasSelector' selector body'' =
  let stringBody = toS body'' in
  case selectorMatches selector stringBody of
        [] -> False
        _  -> True
      
headers :: [(Text, Text)] -> ResponseMatcher
headers xs = defaultMatcher
  { matchHeaders =
      (\hs -> all (`elem` hs) ts, T.pack $ show xs) }
  where ts = map (\(x,y) -> (mk $ toBS x, toBS y)) xs

code' :: Int -> (Int -> Bool, Text)
code' n = ((== 200), T.pack $ show n)

body' :: Text -> (LB.ByteString -> Bool, Text)
body' t = (\x -> encodeUtf8 t `isInfixOf` LB.toStrict x, t)

headers' :: [(Text, Text)] -> ([Header] -> Bool, Text)
headers' xs = (\hs -> all (`elem` hs) ts, T.pack $ show xs)
  where ts = map (\(x,y) -> (mk $ toBS x, toBS y)) xs

toBS :: Text -> ByteString
toBS = encodeUtf8

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
  , actualExpected "selector mismatch:" (selectorError selector body) (toStr' (snd matchCss)) <$  guard (not $ fst matchCss body)
  ]
  where
    selectorError selector body =
      if hasSelector' selector body
         then "Selector doesn't contain that text"
         else "Selector not found."
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
