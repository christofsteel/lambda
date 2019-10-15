module Lambda.ParserHelper
  ( readParen'
  , readChar
  , readUntilOneOf
  , readOnlyChars
  , readWhiteSpaces
  , readWhiteSpaces1
  , replace
  )
where

import           Data.Char

readParen' reader r = do
  ('(', s) <- readChar r
  (rd , t) <- readWhiteSpaces reader s
  (')', u) <- readWhiteSpaces readChar t
  return (rd, u)

readChar :: ReadS Char
readChar "" = fail ""
readChar xs = return (head xs, tail xs)

readOnlyChars :: String -> ReadS Char
readOnlyChars chars "" = fail "empty string"
readOnlyChars chars s | head s `elem` chars = return (head s, tail s)
                      | otherwise           = fail "wrong chars"

readUntilOneOf :: String -> ReadS String
readUntilOneOf cs t = return (span (`notElem` cs) t)

readWhiteSpaces reader xs = reader (dropWhile isSpace xs)

readWhiteSpaces1 reader "" = fail "empty string"
readWhiteSpaces1 reader (x : xs) | isSpace x = reader (dropWhile isSpace xs)
                                 | x == '('  = reader (x : xs)
                                 | otherwise = fail "no whitespaces"

replace :: Char -> Char -> String -> String
replace _ _ [] = []
replace a b (x : xs) | x == a    = b : replace a b xs
                     | otherwise = x : replace a b xs
