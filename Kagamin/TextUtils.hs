{-# LANGUAGE OverloadedStrings #-}
-- | Utilities for working with text.
module Kagamin.TextUtils where
import qualified Data.Text as T
import Data.Char

-- | Was the given message intended for Kagamin?
toKagamin :: T.Text -> T.Text -> Bool
toKagamin kagaid s =
    or [ any (`T.isPrefixOf` s') ["kagamin,", "kagamin:"]
       , kagaid `T.isInfixOf` s]
  where s' = T.toLower s

-- | "baka" -> "b- baka"
stutter :: T.Text -> T.Text
stutter t = T.concat [T.head t `T.cons` "- ",
                      T.singleton (toLower $ T.head t),
                      T.tail t]

-- | Replace all occurrences of @from@ with @to@ in @s@.
replace :: T.Text -> T.Text -> T.Text -> T.Text
replace from to s =
  case T.breakOn from s of
    (_, "") -> s
    (pre, rest) -> T.concat [pre, to, replace from to (T.drop len rest)]
  where
    len = T.length from

-- | Remove any leading or trailing mentions of Kagamin, including whitespace.
stripLeadingTrailingMention :: T.Text -> T.Text -> T.Text
stripLeadingTrailingMention kagaid s
  | any (`T.isPrefixOf` s') ["kagamin,", "kagamin:"] =
    T.dropWhile isSpace $ T.drop 8 s
  | otherwise =
    dropPrefix kagaid $ dropSuffix kagaid $ s
  where s' = T.toLower s

-- | Drop prefix from string, if it is indeed a prefix.
dropPrefix :: T.Text -> T.Text -> T.Text
dropPrefix prefix string = maybe string T.strip $ T.stripPrefix prefix string

-- | Drop suffix from string, if it is indeed a suffix.
dropSuffix :: T.Text -> T.Text -> T.Text
dropSuffix suffix string = maybe string T.strip $ T.stripSuffix suffix string

-- | Remove the @<>@ surrounding URLs.
unCrocodileUrls :: T.Text -> T.Text
unCrocodileUrls s = maybe s T.concat $ go s
  where
    go str = do
      (pre, tmp) <- breakOnEither ["<http://", "<https://"] str
      (url, suf) <- breakOnEither [">"] (T.drop 1 tmp)
      let suf' = T.drop 1 suf
      return $ maybe [pre,url,suf'] ([pre,url]++) $ go suf'

-- | Extract the first URL from a message, if any.
extractUrl :: T.Text -> Maybe T.Text
extractUrl s = do
  (_, suf) <- breakOnEither ["<http://", "<https://"] s
  case T.breakOn ">" (T.drop 1 suf) of
    (_, "")  -> Nothing
    (url, _) -> Just url

-- | Break a string on the first occuring one of several needles.
breakOnEither :: [T.Text] -> T.Text -> Maybe (T.Text, T.Text)
breakOnEither (needle:needles) str =
  case T.breakOn needle str of
    (_, "") -> breakOnEither needles str
    match   -> Just match
breakOnEither [] _ =
  Nothing
