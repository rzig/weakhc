{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Data.Functor (($>), void)
import Text.Parsec
import Text.Parsec.Error (errorMessages, messageString)
import Text.Parsec.Language
import Text.Parsec.String (Parser)
import Text.Parsec.Token
import Control.Applicative (some)
import Data.Maybe (isJust)
import Control.Monad (when)

data Identifier where
  Identifier :: String -> Identifier
  deriving (Show, Eq, Ord)

data Expr
  = WeakTrue
  | WeakFalse
  | WeakNull
  | WeakNumber String
  | WeakString String
  | EIdentifier Identifier
  deriving (Show, Eq, Ord)

data Stmt
  = Print Expr
  | If Expr [Decl]
  | Ret Expr
  | ExprStmt Expr
  deriving (Show, Eq, Ord)

data Decl
  = StmtDecl Stmt
  | OpDecl Identifier Identifier Identifier [Decl]
  | VarDecl Identifier Expr
  | FunDecl Identifier [Identifier] [Decl]
  deriving (Show, Eq, Ord)

notA :: Parser a -> Parser ()
notA p = do
  r <- optionMaybe (lookAhead p)
  when (isJust r) $ fail ""

pIdentifier :: Parser Identifier
pIdentifier = do
  p1 <- letter <?> "Expected identifier to begin with letter"
  p2 <- alphaNum `manyTill` notA alphaNum
  return (Identifier (p1 : p2))

pNumber :: Parser Expr
pNumber = do WeakNumber <$> many1 digit

pString :: Parser Expr
pString = do WeakString <$> stringLiteral (makeTokenParser haskellDef)

pTrue :: Parser Expr
pTrue = do (char 'T' <* notFollowedBy alphaNum) $> WeakTrue

pFalse :: Parser Expr
pFalse = do (char 'F' <* notFollowedBy alphaNum) $> WeakFalse

pNull :: Parser Expr
pNull = do char 'N' $> WeakNull

pExpr :: Parser Expr
pExpr = do
  try pTrue <|> try pFalse <|> try pNull <|> try pString <|> try pNumber <|> EIdentifier <$> pIdentifier

pPrint :: Parser Stmt
pPrint = do char 'p' *> spaces *> (Print <$> pExpr) <* char ';'

pIf :: Parser Stmt
pIf = do
  cond <- char 'i' *> spaces *> between (char '(') (char ')') pExpr
  If cond <$> pBlock

pRet :: Parser Stmt
pRet = do char 'r' *> spaces *> (Ret <$> pExpr) <* char ';'

pExprStmt :: Parser Stmt
pExprStmt = do
  ExprStmt <$> pExpr <* char ';'

pStmt :: Parser Stmt
pStmt = do try pIf <|> try pExprStmt <|> try pPrint <|> pRet

pFunDecl :: Parser Decl
pFunDecl = do
  void (char 'f')
  void spaces
  name <- pIdentifier
  void spaces
  args <- between (char '(') (char ')') (pIdentifier `sepBy` (spaces <* char ',' <* spaces))
  FunDecl name args <$> pBlock

pVarDecl :: Parser Decl
pVarDecl = do
  void (char 'a')
  void spaces
  name <- pIdentifier
  void spaces
  void (char '=')
  void spaces
  value <- pExpr
  void (char ';')
  return (VarDecl name value)

pOpDecl :: Parser Decl
pOpDecl = do
  void (char 'o')
  void spaces
  name <- pIdentifier
  void spaces <* char '('
  left <- pIdentifier
  void spaces <* char ',' <* spaces
  right <- pIdentifier
  void spaces <* char ')'
  OpDecl name left right <$> pBlock

pDecl :: Parser Decl
pDecl = do (StmtDecl <$> try pStmt) <|> try pFunDecl <|> try pVarDecl <|> pOpDecl

pBlock :: Parser [Decl]
pBlock = do spaces *> char '{' *> spaces *> pDecl `sepBy` spaces <* char '}'

stringify :: (Show b) => Either ParseError b -> String
stringify x =
  case x of
    Left a -> show a
    Right b -> show b

main :: IO ()
main = putStrLn (stringify (parse pDecl "file.txt" "o op(x,y){r x;}"))
