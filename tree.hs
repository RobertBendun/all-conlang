-- Copyright (c) 2023 Diana Bendun
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

{-# LANGUAGE TupleSections, LambdaCase #-}

module Main where

import Control.Applicative

type Statement = [Sentance]
data Sentance = Niladic String
              | Monadic String (Maybe Sentance)
              | Dyadic  String (Maybe Sentance) (Maybe Sentance)
              deriving (Show, Eq)

data Arity = Nil | Mon | Dya

arity :: String -> Maybe Int
arity [] = Nothing
arity xs = Just $ case last xs of
  'i' -> 0
  'e' -> 1
  'u' -> 2

first :: (a -> b) -> (a, x) -> (b, x)
first f (a, x) = (f a, x)

second :: (a -> b) -> (x, a) -> (x, b)
second f (x, a) = (x, f a)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> second f <$> p input

instance Applicative Parser where
  pure x = Parser $ Just . (,x)
  Parser p1 <*> Parser p2 = Parser $ \input -> p1 input >>= \(input, f) -> second f <$> p2 input

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  Parser p >>= f = Parser $ \input -> do
    (xs, r) <- p input
    let (Parser b) = f r
    b xs

manyUntil :: Parser a -> Parser end -> Parser ([a], end)
manyUntil many end = (([],) <$> end) <|> (many >>= (\x -> first (x:) <$> manyUntil many end))

separates :: Parser a   -- separator
      -> Parser b   -- elements
      -> Parser [b] -- agregated elements
separates sep element = (:) <$> element <*> many (sep *> element) <|> pure []

unwrap :: Parser (Maybe a) -> Parser a
unwrap (Parser p1) = Parser $ \input -> (p1 input) >>= \case
  (xs, Just x) -> Just (xs, x)
  _ -> Nothing

failWhen :: (a -> Bool) -> Parser a -> Parser a
failWhen f (Parser p) = Parser $ \input -> case (p input) of
  result@(Just (_, a)) | not $ f a -> result
  _ -> Nothing

charP :: Char -> Parser Char
charP c = Parser $ \input -> case input of
  x:xs | x == c -> Just (xs, c)
  _ -> Nothing

statement :: Parser Statement
statement = charP '.' `separates` sentance'
  where
    sentance' :: Parser Sentance
    sentance' = unwrap sentance

letter = foldr1 (<|>) (map charP "aoidnhml")

ws = many $ charP ' '

sentance :: Parser (Maybe Sentance)
sentance = sentanceOrBlank >>= \lhs -> Just <$> dyadic lhs <|> pure lhs

sentanceOrBlank :: Parser (Maybe Sentance)
sentanceOrBlank = Just <$> (ws *> (niladic <|> monadic) <* ws) <|> pure Nothing

-- TODO: This parser does not support ending characters inside the word.
--       Minimal example from the language that is not parsed: ini
endingWith :: Char -> Parser String
endingWith c = fst <$> (manyUntil letter $ charP c)

niladic :: Parser Sentance
niladic = Niladic <$> endingWith 'i'

monadic :: Parser Sentance
monadic = ((Monadic <$> endingWith 'e') <* ws) <*> sentance

dyadic :: Maybe Sentance -> Parser Sentance
dyadic lhs = (((\w -> Dyadic w lhs) <$> endingWith 'u') <* ws) <*> sentance

main :: IO ()
main = return ()
