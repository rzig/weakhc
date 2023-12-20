{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Text.Parsec
import Text.Parsec.Error (errorMessages, messageString)
import Text.Parsec.Language
import Text.Parsec.String (Parser)
import Text.Parsec.Token

data WeakPrimary = WeakTrue | WeakFalse | WeakNull | WeakNumber String | WeakString String | Identifier String deriving (Show, Eq, Ord)

pIdentifier :: Parser WeakPrimary
pIdentifier = do
  p1 <- letter <?> "Expected identifier to begin with letter"
  p2 <- (alphaNum `endBy` spaces) <?> "Identifiers may only contain numbers and letters"
  return (Identifier (p1 : p2))

pNumber :: Parser WeakPrimary
pNumber = do
  n <- many1 digit
  return (WeakNumber n)

pString :: Parser WeakPrimary
pString = do
  p <- stringLiteral (makeTokenParser haskellDef)
  return (WeakString p)

pTrue :: Parser WeakPrimary
pTrue = do
  _ <- char 'T'
  return WeakTrue

pFalse :: Parser WeakPrimary
pFalse = do
  _ <- char 'F'
  return WeakFalse

pNull :: Parser WeakPrimary
pNull = do
  _ <- char 'N'
  return WeakNull

stringify :: (Show b) => Either ParseError b -> String
stringify x =
  case x of
    Left e -> unwords (map messageString (errorMessages e))
    Right b -> show b

main :: IO ()
main = putStrLn (stringify (parse pString "file.txt" "ab\\\"cd\""))
