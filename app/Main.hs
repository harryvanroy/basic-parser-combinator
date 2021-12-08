{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (Alternative (empty, many, some, (<|>)))
import Control.Monad.State (State, evalState, get, put)
import Data.Bifunctor (second)
import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper, toUpper)
import Data.List (intercalate, sortOn)
import System.Environment (getArgs)
import System.Exit (exitFailure)

data Input = Input {inputLoc :: Int, inputStr :: String} deriving (Show, Eq)

data ParserError = ParserError String Int deriving (Show)

newtype Parser a = Parser {runParser :: Input -> Either ParserError (a, Input)}

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
          Left error -> Left error
          Right (value, rest) -> Right (f value, rest)
      )

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = Parser (\inp -> Right (a, inp))

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa =
    Parser
      ( \inp -> case runParser pf inp of
          Left error -> Left error
          Right (f, rest) -> runParser (f <$> pa) rest
      )

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f =
    Parser
      ( \inp -> case runParser p inp of
          Left error -> Left error
          Right (value, rest) -> runParser (f value) rest
      )

instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser (const $ Left (ParserError "empty" 0))

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q =
    Parser
      ( \inp -> case runParser p inp of
          Left _ -> runParser q inp
          Right (value, rest) -> Right (value, rest)
      )

itemP :: Parser Char
itemP =
  Parser
    ( \case
        Input loc [] -> Left (ParserError "Unexpected end of string" loc)
        Input loc (c : cs) -> Right (c, Input (loc + 1) cs)
    )

satP :: (Char -> Bool) -> Parser Char
satP p = do
  x <- itemP
  if p x
    then return x
    else Parser (\(Input loc _) -> Left (ParserError "Failed predicate" loc))

digitP :: Parser Char
digitP =
  Parser
    ( \inp -> case runParser (satP isDigit) inp of
        Left _ -> Left (ParserError ("Expected a digit but found " ++ inputStr inp) (inputLoc inp))
        Right (value, rest) -> Right (value, rest)
    )

withError :: Parser a -> String -> Input -> Either ParserError (a, Input)
withError p msg inp = case runParser p inp of
  Left _ -> Left (ParserError msg (inputLoc inp))
  Right (value, rest) -> Right (value, rest)

charP :: Char -> Parser Char
charP c =
  Parser
    ( \inp ->
        withError
          (satP (== c))
          ("Expected char " ++ show c ++ " but found " ++ inputStr inp)
          inp
    )

stringP :: String -> Parser String
stringP s =
  Parser
    ( \inp ->
        withError
          (traverse charP s)
          ("Expected string " ++ s ++ " but found " ++ inputStr inp)
          inp
    )

intP :: Parser Int
intP =
  do
    charP '-'
    n <- natP
    return (- n)
    <|> natP
  where
    natP = do
      xs <- some digitP
      return (read xs)

withSpace :: Parser a -> Parser a
withSpace p = do
  many (satP isSpace)
  x <- p
  many (satP isSpace)
  return x

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
jsonP = withSpace $ jsonArrayP <|> jsonObjectP

enclose :: (String, String) -> String -> String -> String
enclose brace ind s = fst brace ++ "\n" ++ s ++ "\n" ++ ind ++ snd brace

addQuotes :: String -> String
addQuotes s = "\"" ++ s ++ "\""

indent :: Int -> String
indent lvl = replicate (lvl * 2) ' '

showJsonValue :: Int -> JsonValue -> String
showJsonValue _ (JsonNumber n) = show n
showJsonValue _ (JsonString s) = addQuotes s
showJsonValue _ (JsonBool b) = if b then "true" else "false"
showJsonValue _ JsonNull = "null"
showJsonValue lvl (JsonArray xs) =
  enclose
    ("[", "]")
    (indent lvl)
    (intercalate ",\n" (map ((indent (lvl + 1) ++) . showJsonValue (lvl + 1)) xs))
showJsonValue lvl (JsonObject kvs) =
  enclose
    ("{", "}")
    (indent lvl)
    (intercalate ",\n" (map (showKeyValue (lvl + 1)) kvs))
  where
    showKeyValue lvl (k, v) = indent lvl ++ addQuotes k ++ ": " ++ showJsonValue lvl v

sortKeys :: JsonValue -> JsonValue
sortKeys JsonNull = JsonNull
sortKeys b@(JsonBool _) = b
sortKeys s@(JsonString _) = s
sortKeys n@(JsonNumber _) = n
sortKeys (JsonArray xs) = JsonArray $ sortKeys <$> xs
sortKeys (JsonObject kvs) = JsonObject $ sortOn fst $ second sortKeys <$> kvs

parseJsonFile :: FilePath -> IO JsonValue
parseJsonFile filePath = do
  fileContents <- readFile filePath
  case runParser jsonP (Input 0 fileContents) of
    Left (ParserError msg loc) -> putStrLn ("Parser failed at char " ++ show loc ++ ": " ++ msg) >> exitFailure
    Right (v, _) -> return $ sortKeys v

main :: IO ()
main = do
  args <- getArgs
  jsonResult <- parseJsonFile $ head args
  writeFile (head args) (showJsonValue 0 jsonResult)
