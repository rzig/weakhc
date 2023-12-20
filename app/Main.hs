{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Text.Parsec
import Text.Parsec.Error (errorMessages, messageString)
import Text.Parsec.Language
import Text.Parsec.String (Parser)
import Text.Parsec.Token

data Expr
  = WeakTrue
  | WeakFalse
  | WeakNull
  | WeakNumber String
  | WeakString String
  | Identifier String
  | Arguments [Expr]
  deriving (Show, Eq, Ord)

data Stmt
  = Print Expr
  | If Expr [Decl]
  | Ret Expr
  | ExprStmt Expr
  deriving (Show, Eq, Ord)

data Decl =
  StmtDecl Stmt |
  OpDecl Expr Expr Expr [Stmt] |
  VarDecl Expr Expr |
  FunDecl Expr Expr
  deriving (Show, Eq, Ord)

pIdentifier :: Parser Expr
pIdentifier = do
  p1 <- letter <?> "Expected identifier to begin with letter"
  p2 <- many1 alphaNum
  -- p2 <- (alphaNum `endBy` (spaces <|> try (string ";"))) <?> "Identifiers may only contain numbers and letters"
  return (Identifier (p1 : p2))

pNumber :: Parser Expr
pNumber = do WeakNumber <$> many1 digit

pString :: Parser Expr
pString = do WeakString <$> stringLiteral (makeTokenParser haskellDef)

pTrue :: Parser Expr
pTrue = do
  _ <- char 'T' <* notFollowedBy alphaNum
  return WeakTrue

pFalse :: Parser Expr
pFalse = do
  _ <- char 'F' <* notFollowedBy alphaNum
  return WeakFalse

pNull :: Parser Expr
pNull = do
  _ <- char 'N'
  return WeakNull

pExpr :: Parser Expr
pExpr = do
  try pTrue <|> try pFalse <|> try pNull <|> try pString <|> try pNumber <|> try pIdentifier

pPrint :: Parser Stmt
pPrint = do char 'p' *> spaces *> (Print <$> pExpr) <* char ';'

pIf :: Parser Stmt
pIf = do
  cond <- char 'i' *> spaces *> between (char '(') (char ')') pExpr
  If cond <$> pBlock

pExprStmt :: Parser Stmt
pExprStmt = do
  ExprStmt <$> pExpr <* char ';'

pStmt :: Parser Stmt
pStmt = do try pIf <|> try pExprStmt <|> try pPrint

pDecl :: Parser Decl
pDecl = do StmtDecl <$> try pStmt

pBlock :: Parser [Decl]
pBlock = do spaces *> char '{' *> spaces *> pDecl `sepBy` spaces <* char '}'

stringify :: (Show b) => Either ParseError b -> String
stringify x =
  case x of
    Left e -> unwords (map messageString (errorMessages e))
    Right b -> show b

main :: IO ()
main = putStrLn (stringify (parse pDecl "file.txt" "i(T) {p 123;}"))
