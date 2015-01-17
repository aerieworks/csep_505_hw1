module Interp where

import SExp

-- Operations to support.
-- Concrete syntax:
-- <op> ::= + | * | = | <
data BinOp = Add | Mult | Equal | Lt deriving (Eq, Show)

-- Expressions.
-- Concrete syntax:
-- <e> ::= <number>
--       | true
--       | false
--       | (<op> <e> <e>)
--       | (if <e> <e> <e>)
data Expr = NumE Integer
          | BoolE Bool
          | BinOpE BinOp Expr Expr
          | IfE Expr Expr Expr
          deriving (Eq, Show)

parseExpr :: SExp -> Result Expr
parseExpr sexp =
  case sexp of
    NumS n                             -> Ok (NumE n)
    IdS "true"                         -> Ok (BoolE True)
    IdS "false"                        -> Ok (BoolE False)
    ListS ((IdS "if"):cond:true:false:[]) -> parseIfE (parseExpr cond) (parseExpr true) (parseExpr false)
    ListS ((IdS op):x:y:[])               -> parseBinE (parseBinOp op) (parseExpr x) (parseExpr y)

parseBinE :: Result BinOp -> Result Expr -> Result Expr -> Result Expr
parseBinE (Err str) _ _ = Err str
parseBinE _ (Err str) _ = Err str
parseBinE _ _ (Err str) = Err str
parseBinE (Ok op) (Ok x) (Ok y) = Ok (BinOpE op x y)

parseBinOp :: String -> Result BinOp
parseBinOp str =
  case str of
    "+" -> Ok Add
    "*" -> Ok Mult
    "=" -> Ok Equal
    "<" -> Ok Lt
    otherwise -> Err ("Unknown binary operator '" ++ str ++ "'.")

parseIfE :: Result Expr -> Result Expr -> Result Expr -> Result Expr
parseIfE (Err str) _ _ = Err str
parseIfE _ (Err str) _ = Err str
parseIfE _ _ (Err str) = Err str
parseIfE (Ok x) (Ok y) (Ok z) = Ok (IfE x y z)

validParseExamples = [
  ("non-trivial example", "(if (= (* 2 3) (+ 5 1)) 7 10)",
   IfE (BinOpE Equal (BinOpE Mult (NumE 2) (NumE 3))
        (BinOpE Add (NumE 5) (NumE 1))) (NumE 7) (NumE 10))
  -- Feel free to add your own examples ...
  ]

checkValidParseExample (description, str, expected) =
  (description,
   case parseSExp (tokenize str) of
    Ok (sexp, []) -> parseExpr sexp == Ok expected
    _ -> False)

interp :: Expr -> Result Expr
interp expr = Err "unimplemented"
