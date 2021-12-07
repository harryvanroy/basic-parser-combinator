{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (Alternative (empty, many, some, (<|>)))
import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper, toUpper)
import Data.List (intercalate)
import System.Environment (getArgs)

type Input = String

newtype Parser a = Parser {runParser :: Input -> Maybe (a, Input)}

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonNumber Int
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

instance Functor Parser where
  -- fmap :: (a->b) -> Parser a -> Parser b
  fmap f p =
    Parser
      ( \inp -> case runParser p inp of
          Nothing -> Nothing
          Just (a, rest) -> Just (f a, rest)
      )

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = Parser (\inp -> Just (a, inp))

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa =
    Parser
      ( \inp -> case runParser pf inp of
          Nothing -> Nothing
          Just (f, rest) -> runParser (f <$> pa) rest
      )

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f =
    Parser
      ( \inp -> case runParser p inp of
          Nothing -> Nothing
          Just (a, rest) -> runParser (f a) rest
      )

instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser (const Nothing)

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q =
    Parser
      ( \inp -> case runParser p inp of
          Nothing -> runParser q inp
          Just (a, rest) -> Just (a, rest)
      )

itemP :: Parser Char
itemP =
  Parser
    ( \case
        [] -> Nothing
        (c : cs) -> Just (c, cs)
    )

satP :: (Char -> Bool) -> Parser Char
satP p = do
  x <- itemP
  if p x then return x else empty

digitP :: Parser Char
digitP = satP isDigit

charP :: Char -> Parser Char
charP x = satP (== x)

stringP :: String -> Parser String
stringP [] = empty
stringP (x : xs) = do
  charP x
  stringP xs
  return (x : xs)

natP :: Parser Int
natP =
  do
    xs <- some digitP
    return (read xs)

intP :: Parser Int
intP =
  do
    charP '-'
    n <- natP
    return (- n)
    <|> natP

spaceP :: Parser ()
spaceP = do
  many (satP isSpace)
  return ()

withSpace :: Parser a -> Parser a
withSpace p = do
  spaceP
  v <- p
  spaceP
  return v

sepBy' :: Parser a -> Parser b -> Parser [b]
sepBy' sep p = do
  x <- p
  xs <- many (sep >> p)
  return (x : xs)

jsonNullP :: Parser JsonValue
jsonNullP = JsonNull <$ stringP "null"

jsonBoolP :: Parser JsonValue
jsonBoolP = trueP <|> falseP
  where
    trueP = JsonBool True <$ stringP "true"
    falseP = JsonBool False <$ stringP "false"

jsonNumberP :: Parser JsonValue
jsonNumberP = JsonNumber <$> intP

stringQuotesP :: Parser String
stringQuotesP = do
  charP '"'
  xs <- many (satP (/= '"'))
  charP '"'
  return xs

jsonStringP :: Parser JsonValue
jsonStringP = JsonString <$> stringQuotesP

jsonArrayP :: Parser JsonValue
jsonArrayP = do
  charP '['
  xs <- withSpace $ sepBy' (withSpace $ charP ',') jsonValueP <|> pure []
  charP ']'
  return $ JsonArray xs

jsonObjectP :: Parser JsonValue
jsonObjectP = do
  charP '{'
  kvs <- withSpace $ sepBy' (withSpace $ charP ',') jsonKeyValueP <|> pure []
  charP '}'
  return $ JsonObject kvs

jsonKeyValueP :: Parser (String, JsonValue)
jsonKeyValueP = do
  k <- stringQuotesP
  withSpace $ charP ':'
  v <- jsonValueP
  return (k, v)

jsonValueP :: Parser JsonValue
jsonValueP =
  jsonNullP
    <|> jsonBoolP
    <|> jsonStringP
    <|> jsonNumberP
    <|> jsonArrayP
    <|> jsonObjectP

jsonP :: Parser JsonValue
jsonP = withSpace $ jsonObjectP <|> jsonArrayP

parseJsonFile :: FilePath -> IO JsonValue
parseJsonFile filePath = do
  x <- readFile filePath
  case runParser jsonP x of
    Nothing -> error "parse error"
    Just (v, _) -> return v

showJsonValue :: JsonValue -> String
showJsonValue jv = case jv of
  JsonNumber n -> show n
  JsonString s -> addQuotes s
  JsonBool b -> if b then "true" else "false"
  JsonNull -> "null"
  JsonArray xs -> "[" ++ intercalate "," (map showJsonValue xs) ++ "]"
  JsonObject kvs ->
    "{\n\t"
      ++ intercalate
        ",\n"
        (map (\(k, v) -> addQuotes k ++ ": " ++ showJsonValue v) kvs)
      ++ "\n}"
  where
    addQuotes s = "\"" ++ s ++ "\""

main :: IO ()
main = do
  args <- getArgs
  jsonResult <- parseJsonFile $ head args
  writeFile (head args) (showJsonValue jsonResult)
