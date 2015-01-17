module SExp (SExp(NumS, IdS, ListS), parseSExp, tokenize, Result(Ok, Err)) where

import Token

-- Converts a string into a list of tokens.
tokenize :: String -> [Token]
tokenize str = case parseToken str of
  Nothing -> []
  Just x  -> (fst x):(tokenize (snd x))

-- S-expression data definition.
data SExp = NumS Integer -- numeric expression
          | IdS String -- identifier
          | ListS [SExp] -- list of S-expressions
          deriving Eq

-- Show SExps in surface syntax.
instance Show SExp where
  show (NumS n) = show n
  show (IdS name) = name
  show (ListS sexps) = "(" ++ (unwords (map show sexps)) ++ ")"

-- Type for results of functions that can fail, such as parsing.
data Result a = Ok a -- Success
              | Err String -- Error with description
              deriving (Eq, Show)

-- Attempts to parse an S-expression from a list of tokens.
-- If successful, returns:
--    Ok (<parsed S-expression>, <remaining tokens>)
-- If not, returns:
--    Err <string describing problem encountered>
parseSExp :: [Token] -> Result (SExp, [Token])
parseSExp [] = (Err "Empty token list.")
parseSExp (t:ts) =
  case t of
    NumTok t  -> Ok (NumS t, ts)
    IdTok t   -> Ok (IdS t, ts)
    Open b    -> parseSExpList b [] ts
    otherwise -> Err "Invalid S-expression."

parseSExpList :: Brace -> [SExp] -> [Token] -> Result (SExp, [Token])
parseSExpList b ss ts =
  case ts of
    []           -> Err "Missing close brace."
    (Close c:us) -> if c == b then Ok (ListS (reverse ss), us) else Err "Mismatched brace types."
    otherwise    -> case parseSExp ts of
                      Err str    -> Err str
                      Ok (s, us) -> parseSExpList b (s:ss) us

-- Examples that should parse.
validExamples = [
  ("empty list", "()", ListS []),
  ("single id", "true", IdS "true"),
  ("positive num", "1234", NumS 1234),
  ("negative num", "-1234", NumS (-1234)),
  ("mixed list", "(foo () 4 (7 false))",
   ListS [IdS "foo", ListS [], NumS 4, ListS [NumS 7, IdS "false"]])
  ]

-- Examples that should not parse.
invalidExamples = [
  ("empty", ""),
  ("close without open", ")"),
  ("no close", "(3 4"),
  ("mismatched brace types", "(foo bar]")
  ]

-- Check a single valid example.
checkValid (description, input, expected) =
  (description, (parseSExp (tokenize input)) == Ok (expected, []))

-- Check a single invalid example.
checkInvalid (description, input) =
  (description, case parseSExp (tokenize input) of
                 -- Just check that it failed; don't try to match the
                 -- specific error message.
                 Err _ -> True
                 _ -> False)
