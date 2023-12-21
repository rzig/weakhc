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

neg :: Parser a -> Parser Integer
neg p = do
  r <- optionMaybe (lookAhead p)
  if isJust r
    then fail ""
    else return 1
  -- fail ""
  -- return (case r of
    -- Nothing -> 1
    -- Just _ -> (fail "not found"))

pIdentifier :: Parser Identifier
pIdentifier = do
  p1 <- letter <?> "Expected identifier to begin with letter"
  p2 <- alphaNum `manyTill` neg alphaNum
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

pFunction :: Parser Decl
pFunction = do
  void (char 'f')
  void spaces
  name <- pIdentifier
  void spaces
  args <- between (char '(') (char ')') (pIdentifier `sepBy` (spaces <* char ',' <* spaces))
  FunDecl name args <$> pBlock

pDecl :: Parser Decl
pDecl = do (StmtDecl <$> try pStmt) <|> try pFunction

pBlock :: Parser [Decl]
pBlock = do spaces *> char '{' *> spaces *> pDecl `sepBy` spaces <* char '}'

stringify :: (Show b) => Either ParseError b -> String
stringify x =
  case x of
    Left a -> show a
    Right b -> show b

main :: IO ()
main = putStrLn (stringify (parse pDecl "file.txt" "f myfunc(x,y){p a;}"))
