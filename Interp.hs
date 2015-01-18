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
    NumS n                                -> Ok (NumE n)
    IdS "true"                            -> Ok (BoolE True)
    IdS "false"                           -> Ok (BoolE False)
    ListS ((IdS "if"):cond:true:false:[]) -> parseIfE cond true false
    ListS ((IdS op):x:y:[])               -> parseBinE op x y
    otherwise                             -> Err "Invalid syntax."

parseBinE :: String -> SExp -> SExp -> Result Expr
parseBinE opStr xSExp ySExp =
  case parseBinOp opStr of
    Err str   -> Err str
    Ok op     -> case parseNumExpr xSExp of
                  Err str -> Err str
                  Ok x    -> case parseNumExpr ySExp of
                               Err str -> Err str
                               Ok y    -> Ok (BinOpE op x y)

parseBinOp :: String -> Result BinOp
parseBinOp str =
  case str of
    "+" -> Ok Add
    "*" -> Ok Mult
    "=" -> Ok Equal
    "<" -> Ok Lt
    otherwise -> Err ("Unknown binary operator '" ++ str ++ "'.")

parseNumExpr :: SExp -> Result Expr
parseNumExpr sexp =
  let exp = parseExpr sexp
  in case exp of
    Ok (BinOpE Add _ _)  -> exp
    Ok (BinOpE Mult _ _) -> exp
    Ok (NumE _)          -> exp
    Ok _                 -> Err "Expected numeric expression."
    otherwise            -> exp

parseBoolExpr :: SExp -> Result Expr
parseBoolExpr sexp =
  let exp = parseExpr sexp
  in case exp of
    Ok (IfE _ _ _)        -> exp
    Ok (BoolE _)          -> exp
    Ok (BinOpE Equal _ _) -> exp
    Ok (BinOpE Lt _ _)    -> exp
    Ok _                  -> Err "Expected boolean expression."
    otherwise             -> exp

parseIfE :: SExp -> SExp -> SExp -> Result Expr
parseIfE condSExp trueSExp falseSExp =
  case parseBoolExpr condSExp of
    Err str -> Err str
    Ok cond -> case parseNumExpr trueSExp of
                 Err str -> Err str
                 Ok true -> case parseNumExpr falseSExp of
                               Err str  -> Err str
                               Ok false -> Ok (IfE cond true false)

validParseExamples = [
  ("non-trivial example", "(if (= (* 2 3) (+ 5 1)) 7 10)",
   IfE (BinOpE Equal (BinOpE Mult (NumE 2) (NumE 3))
        (BinOpE Add (NumE 5) (NumE 1))) (NumE 7) (NumE 10)),
  ("number", "5", NumE 5),
  ("boolean true", "true", BoolE True),
  ("boolean false", "false", BoolE False),
  ("arithmetic", "(* (+ 4 7) (+ 3 5))", BinOpE Mult (BinOpE Add (NumE 4) (NumE 7))
        (BinOpE Add (NumE 3) (NumE 5))),
  ("negative numbers", "(+ 5 -4)", BinOpE Add (NumE 5) (NumE (-4))),
  ("comparison", "(= 1 2)", BinOpE Equal (NumE 1) (NumE 2)),
  ("if statement", "(if (= (+ 2 4) (* 3 2)) (+ 3 (* 5 2)) 7)", IfE (BinOpE Equal
    (BinOpE Add (NumE 2) (NumE 4)) (BinOpE Mult (NumE 3) (NumE 2)))
    (BinOpE Add (NumE 3) (BinOpE Mult (NumE 5) (NumE 2))) (NumE 7))
  -- Feel free to add your own examples ...
  ]

invalidParseExamples = [
  ("missing right operand", "(+ 5)"),
  ("missing left operand", "(+)"),
  ("missing else clause", "(if (= x y) z)"),
  ("missing if clause", "(if (= x y))"),
  ("missing conditional", "(if)"),
  ("Type mismatch in addition left operand", "(+ (= 1 2) 3)"),
  ("Type mismatch in addition right operand", "(+ 1 (= 2 3))"),
  ("Type mismatch in multiplication left operand", "(* (= 1 2) 3)"),
  ("Type mismatch in multiplication right operand", "(* 1 (= 2 3))"),
  ("Type mismatch in equals left operand", "(= (= 1 2) 3)"),
  ("Type mismatch in equals right operand", "(= 1 (= 2 3))"),
  ("Type mismatch in less than left operand", "(< (= 1 2) 3)"),
  ("Type mismatch in less than right operand", "(< 1 (= 2 3))"),
  ("Type mismatch in conditional", "(if (+ 5 3) 1 2)")
  ]

checkValidParseExample (description, str, expected) =
  (description,
   case parseSExp (tokenize str) of
    Ok (sexp, []) -> parseExpr sexp == Ok expected
    _ -> False)

checkInvalidParseExample (description, input) =
  case parseSExp (tokenize input) of
    Err str      -> (description, False, str)
    Ok (sexp, _) -> case parseExpr sexp of
                      Err str -> (description, True, str)
                      Ok exp  -> (description, False, show exp)

parse :: String -> Result Expr
parse input =
  case parseSExp (tokenize input) of
    Ok (sexp, _) -> parseExpr sexp
    Err str      -> Err str

interp :: Expr -> Result Expr
interp expr = Err "unimplemented"
