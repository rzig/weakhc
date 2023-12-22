{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}


module Main (main) where

import Control.Applicative (some)
import Control.Monad (when)
import Data.Functor (void, ($>))
import Data.Maybe (isJust, fromMaybe)
import Text.Parsec
import Text.Parsec.Error (errorMessages, messageString)
import Text.Parsec.Language
import Text.Parsec.String (Parser)
import Text.Parsec.Token hiding (lexeme)
import Text.Parsec.Expr (buildExpressionParser, Assoc (AssocLeft))

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
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Exp Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Eq Expr Expr
  | Neq Expr Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | MatMul Expr Expr
  | ShapeAs Expr Expr
  | ShapeOf Expr
  | NegativeOf Expr
  | NotOf Expr
  | FunctionCall Identifier [Expr]
  | InlineArray [Expr]
  | ArrayAccess Expr [Expr]
  | Assign Identifier [Expr] Expr
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

inParens :: Parser a -> Parser a
inParens = between (char '(') (char ')')

inBrackets :: Parser a -> Parser a
inBrackets = between (char '[') (char ']')

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

pPrimary :: Parser Expr
pPrimary = do
  try pTrue <|> try pFalse <|> try pNull <|> try pString <|> try pNumber <|> EIdentifier <$> pIdentifier <|> inParens pExpr

pFunctionCall :: Parser Expr
pFunctionCall = do
    name <- lexeme pIdentifier
    FunctionCall name <$> inParens (lexeme pExpr `sepBy` char ',')

pFunction :: Parser Expr
pFunction = do try pFunctionCall <|> pPrimary

pDirectArrayAccess :: Parser Expr
pDirectArrayAccess = do
  name <- pFunction
  indices <- inBrackets (lexeme pExpr `sepBy` char ',')
  return (ArrayAccess name indices)

pArrayAccess :: Parser Expr
pArrayAccess = do try pDirectArrayAccess <|> pFunction

pUnaryOf :: String -> (Expr -> Expr) -> Parser Expr
pUnaryOf c e = e <$> (lexeme (string c) *> pUnary)

pDirectUnary :: Parser Expr
pDirectUnary = do try (pUnaryOf "-" NegativeOf) <|> try (pUnaryOf "!" NotOf) <|> try (pUnaryOf "s " ShapeOf)

pUnary :: Parser Expr
pUnary = do try pDirectUnary <|> pArrayAccess

-- pInfix s m r = do
  -- left <- lexeme s
  -- void m
  -- r left <$> lexeme s

-- fold :: (a -> a -> b) -> [a] -> b
-- fold op (x:xs) = case xs of
  -- [] -> x
  -- _ -> x `op` fold xs

-- pFactor = do
  -- fold (<|>) [try (pInfix pFactor (char '*') Mult)]
pFactor :: Parser Expr
pFactor = chainl1 (lexeme pUnary) op
  where op = Mult <$ char '*'
         <|> Div  <$ char '/'
         <|> MatMul <$ char '@'
         <|> ShapeAs <$ string "sa "
         <|> Exp <$ char '^'

pTerm :: Parser Expr
pTerm = chainl1 (lexeme pFactor) op
  where op = Add <$ char '+' <|> Sub <$ char '-'

pComparison :: Parser Expr
pComparison = chainl1 (lexeme pTerm) op
  where op = Lt <$ char '<' <|> Gt <$ char '>'

pEquality :: Parser Expr
pEquality = chainl1 (lexeme pComparison) op
  where op = Eq <$ string "==" <|> Neq <$ string "!="

pLogicAnd :: Parser Expr
pLogicAnd = chainl1 (lexeme pEquality) (And <$ (char 'A' <* notFollowedBy alphaNum))

pLogicOr :: Parser Expr
pLogicOr = chainl1 (lexeme pLogicAnd) (Or <$ (char 'O' <* notFollowedBy alphaNum))

pDirectAssignment :: Parser Expr
pDirectAssignment = do
  v <- lexeme pIdentifier
  args <- optionMaybe (inBrackets (pExpr `sepBy` spaces))
  void (char '=')
  Assign v (fromMaybe [] args) <$> lexeme pExpr

pExpr :: Parser Expr
pExpr = do try pDirectAssignment <|> try pLogicOr

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
main = putStrLn (stringify (parse pExpr "file.txt" "m = (3 + 4) * 2"))
