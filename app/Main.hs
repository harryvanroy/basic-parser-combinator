{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isUpper, toUpper)

type Input = String

newtype Parser a = Parser {runParser :: Input -> Maybe (a, Input)}

-- >>> runParser (fmap toUpper item) "abc"
-- Just ('A',"bc")

-- >>> runParser (fmap toUpper item) ""
-- Nothing

instance Functor Parser where
  -- fmap :: (a->b) -> Parser a -> Parser b
  fmap f p =
    Parser
      ( \inp -> case runParser p inp of
          Nothing -> Nothing
          Just (a, rest) -> Just (f a, rest)
      )

--- >>> runParser (pure toUpper <*> item ) "abc"
-- Just ('A',"bc")

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

item :: Parser Char
item =
  Parser
    ( \case
        [] -> Nothing
        (c : cs) -> Just (c, cs)
    )

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphaNum :: Parser Char
alphaNum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

main :: IO ()
main = putStrLn "Hello, Haskell!"
