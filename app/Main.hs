{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Data.Functor (($>), void)
import Text.Parsec
import Text.Parsec.Error (errorMessages, messageString)
import Text.Parsec.Language
import Text.Parsec.String (Parser)
import Text.Parsec.Token hiding (lexeme)
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

lexeme :: Parser a -> Parser a
lexeme p = do spaces *> p <* spaces

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
pPrint = do char 'p' *> (Print <$> lexeme pExpr) <* char ';'

pIf :: Parser Stmt
pIf = do
  cond <- lexeme (char 'i') *> between (char '(') (char ')') (lexeme pExpr)
  If cond <$> pBlock

pRet :: Parser Stmt
pRet = do lexeme (char 'r') *> (Ret <$> lexeme pExpr) <* char ';'

pExprStmt :: Parser Stmt
pExprStmt = do
  ExprStmt <$> lexeme pExpr <* char ';'

pStmt :: Parser Stmt
pStmt = do try pIf <|> try pExprStmt <|> try pPrint <|> pRet

pFunDecl :: Parser Decl
pFunDecl = do
  void (lexeme (char 'f'))
  name <- lexeme pIdentifier
  args <- between (char '(') (char ')') (lexeme pIdentifier `sepBy` char ',')
  FunDecl name args <$> pBlock

pVarDecl :: Parser Decl
pVarDecl = do
  void (lexeme (char 'a'))
  name <- pIdentifier
  void (lexeme (char '='))
  value <- pExpr
  void (lexeme (char ';'))
  return (VarDecl name value)

pOpDecl :: Parser Decl
pOpDecl = do
  void (lexeme (char 'o'))
  name <- pIdentifier
  void (lexeme (char '('))
  left <- pIdentifier
  void (lexeme (char ','))
  right <- pIdentifier
  void (lexeme (char ')'))
  OpDecl name left right <$> pBlock

pDecl :: Parser Decl
pDecl = do (StmtDecl <$> try pStmt) <|> try pFunDecl <|> try pVarDecl <|> pOpDecl

pBlock :: Parser [Decl]
pBlock = do lexeme (char '{') *> pDecl `sepBy` spaces <* lexeme (char '}')

stringify :: (Show b) => Either ParseError b -> String
stringify x =
  case x of
    Left a -> show a
    Right b -> show b

main :: IO ()
main = putStrLn (stringify (parse pDecl "file.txt" "f op(x,  y ){p    x ; aa   ;}"))
