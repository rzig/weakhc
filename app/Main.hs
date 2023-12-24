{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Control.Applicative (some, (<**>))
import Control.Arrow (ArrowChoice (right), left)
import Control.Monad (when)
import Control.Monad.Trans.Cont
import Data.Functor (void, ($>))
import Data.Maybe (fromMaybe, isJust)
import Text.Parsec
import Text.Parsec.Error (errorMessages, messageString)
import Text.Parsec.Expr (Assoc (AssocLeft), buildExpressionParser)
import Text.Parsec.Language
import Text.Parsec.String (Parser)
import Text.Parsec.Token hiding (lexeme)

data Identifier where
  Identifier :: String -> Identifier
  deriving (Show, Eq, Ord)

name :: Identifier -> String
name (Identifier x) = x

data Primitive a = WeakTrue | WeakFalse | WeakNull | WeakNumber a | WeakString a deriving (Show, Eq, Ord)

data BinOpType = Add | Sub | Mult | Div | Exp | And | Or | Eq | Neq | Lt | Gt | MatMul | ShapeAs deriving (Show, Eq, Ord)

data UnaryOpType = ShapeOf | NegativeOf | NotOf deriving (Show, Eq, Ord)

data Expr
  = EPrimitive (Primitive String)
  | EIdentifier Identifier
  | BinOp BinOpType Expr Expr
  | UnaryOp UnaryOpType Expr
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
  let iden = p1 : p2
  if iden == "a" then fail "reserved" else return (Identifier iden)

pNumber :: Parser Expr
pNumber = do EPrimitive . WeakNumber <$> many1 digit

pString :: Parser Expr
pString = do EPrimitive . WeakString <$> stringLiteral (makeTokenParser haskellDef)

pTrue :: Parser Expr
pTrue = do (char 'T' <* notFollowedBy alphaNum) $> EPrimitive WeakTrue

pFalse :: Parser Expr
pFalse = do (char 'F' <* notFollowedBy alphaNum) $> EPrimitive WeakFalse

pNull :: Parser Expr
pNull = do char 'N' $> EPrimitive WeakNull

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

pCreationArrayAccess :: Parser Expr
pCreationArrayAccess = do
  indices <- inBrackets (lexeme pExpr `sepBy` char ',')
  return (InlineArray indices)

pArrayAccess :: Parser Expr
pArrayAccess = do try pDirectArrayAccess <|> try pCreationArrayAccess <|> try pFunction

pUnaryOf :: String -> (Expr -> Expr) -> Parser Expr
pUnaryOf c e = e <$> (lexeme (string c) *> pUnary)

pDirectUnary :: Parser Expr
pDirectUnary = do try (pUnaryOf "-" (UnaryOp NegativeOf)) <|> try (pUnaryOf "!" (UnaryOp NotOf)) <|> try (pUnaryOf "s " (UnaryOp ShapeOf))

pUnary :: Parser Expr
pUnary = do try pDirectUnary <|> pArrayAccess

pFactor :: Parser Expr
pFactor = chainl1 (lexeme pUnary) op
  where
    op =
      BinOp Mult <$ char '*'
        <|> BinOp Div <$ char '/'
        <|> BinOp MatMul <$ char '@'
        <|> BinOp ShapeAs <$ (string "sa" <* notFollowedBy alphaNum)
        <|> BinOp Exp <$ char '^'

pTerm :: Parser Expr
pTerm = chainl1 (lexeme pFactor) op
  where
    op = BinOp Add <$ char '+' <|> BinOp Sub <$ char '-'

pComparison :: Parser Expr
pComparison = chainl1 (lexeme pTerm) op
  where
    op = BinOp Lt <$ char '<' <|> BinOp Gt <$ char '>'

pEquality :: Parser Expr
pEquality = chainl1 (lexeme pComparison) op
  where
    op = BinOp Eq <$ string "==" <|> BinOp Neq <$ string "!="

pLogicAnd :: Parser Expr
pLogicAnd = chainl1 (lexeme pEquality) (BinOp And <$ (char 'A' <* notFollowedBy alphaNum))

pLogicOr :: Parser Expr
pLogicOr = chainl1 (lexeme pLogicAnd) (BinOp Or <$ (char 'O' <* notFollowedBy alphaNum))

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

pProgram :: Parser [Decl]
pProgram = do pDecl `sepBy` spaces

stringify :: (Show b) => Either ParseError b -> String
stringify x =
  case x of
    Left a -> show a
    Right b -> show b

data Environment = Environment
  { funcs :: [Identifier],
    vars :: [Identifier],
    errors :: [String]
  }
  deriving (Show)

putError :: String -> Environment -> Environment
putError s e = Environment (funcs e) (vars e) (errors e <> [s])

putFunc :: Identifier -> Environment -> Environment
putFunc f e = Environment (funcs e <> [f]) (vars e) (errors e)

putVar :: Identifier -> Environment -> Environment
putVar v e = Environment (funcs e) (vars e <> [v]) (errors e)

addErrorsTo :: Environment -> Environment -> Environment
addErrorsTo e e' = Environment (funcs e') (vars e') (errors e ++ errors e')

withoutErrors :: Environment -> Environment
withoutErrors e = Environment (funcs e) (vars e) []

class Validatable a where
  validate :: a -> Environment -> Cont r Environment

instance Validatable Identifier where
  validate iden e = do
    let exists = iden `elem` vars e || iden `elem` funcs e
    if not exists then return (putError ("Could not resolve identifier " ++ name iden) e) else return e

instance Validatable Expr where
  validate (EPrimitive _) e = do return e
  validate (EIdentifier id) e = do validate id e
  validate (BinOp _ l r) e = do validate l e >>= validate r
  validate (UnaryOp _ o) e = do validate o e
  validate (FunctionCall f []) e = do validate f e
  validate (FunctionCall f (x : xs)) e = validate x e >>= validate (FunctionCall f xs)
  validate (InlineArray []) e = do return e
  validate (InlineArray (x : xs)) e = do validate x e >>= validate (InlineArray xs)
  validate (ArrayAccess a []) e = do validate a e
  validate (ArrayAccess a (x : xs)) e = do validate x e >>= validate (ArrayAccess a xs)
  validate (Assign id [] r) e = do validate id e >>= validate r
  validate (Assign id (x : xs) r) e = do validate x e >>= validate (Assign id xs r)

instance Validatable Stmt where
  validate (Print p) e = do validate p e
  validate (If c []) e = do validate c e
  validate (Ret v) e = do validate v e
  validate (ExprStmt ex) e = do validate ex e

instance Validatable [Decl] where
  validate [] e = do return e
  validate (x : xs) e = do validate x e >>= validate xs

instance Validatable Decl where
  validate (StmtDecl s) e = do validate s e
  validate (OpDecl name lp rp body) e = do
    x <- validate body (Environment [name] [lp, rp] [])
    return (addErrorsTo x e)
  validate (VarDecl l r) e = do
    x <- validate r (withoutErrors e)
    return (putVar l (addErrorsTo x e))
  validate (FunDecl f p b) e = do
    x <- validate b (Environment [f] p [])
    return (putFunc f (addErrorsTo x e))

validateAst :: [Decl] -> Environment -> Environment
validateAst a e = runCont (validate a e) id

main :: IO ()
main = do
  let code = "f g(x,y) {p x + y;} a b = 1; a c = b; g(b,c);"
  let ast = parse pProgram "file.txt" code
  let validated = right (\x -> validateAst x (Environment [] [] [])) ast
  putStrLn (stringify ast)
  putStrLn (stringify validated)
