{
module CLikeLexer where

import Prelude hiding (lex)
}

%wrapper "monad"

$digit = 0-9        --digit
$alpha = [a-zA-Z]   -- alphabetics

tokens :-
    $white+     ;
    "//".*      ;
    $digit+     { lex (TokenInt . read) }
    "+"         { lex' TokenPlus }
    "-"         { lex' TokenMinus }
    "*"         { lex' TokenTimes }
    "/"         { lex' TokenDivide }
    "%"         { lex' TokenModulus }
    "<<"        { lex' TokenLShift }
    ">>"        { lex' TokenRShift }
    "=="        { lex' TokenEqualsEquals }
    "="         { lex' TokenEquals }
    "<"         { lex' TokenLThan }
    ">"         { lex' TokenGThan }
    "("         { lex' TokenLParen }
    ")"         { lex' TokenRParen }
    "{"         { lex' TokenLCurly }
    "}"         { lex' TokenRCurly }
    ","         { lex' TokenComma }
    ";"         { lex' TokenSemicol }
    if          { lex' TokenIf }
    else        { lex' TokenElse }
    return      { lex' TokenReturn }
    int         { lex' TokenIntType }
    const       { lex' TokenConstType }
    [a-zA-Z_][a-zA-Z0-9_]*  { lex (TokenId . read) }

{

-- The token type:
data Token =
      TokenId String
    | TokenInt Int
    | TokenPlus
    | TokenMinus
    | TokenTimes
    | TokenDivide
    | TokenModulus
    | TokenLShift
    | TokenRShift
    | TokenEqualsEquals
    | TokenEquals
    | TokenLThan
    | TokenGThan
    | TokenLParen
    | TokenRParen
    | TokenLCurly
    | TokenRCurly
    | TokenComma
    | TokenSemicol
    | TokenIf
    | TokenElse
    | TokenReturn
    | TokenIntType
    | TokenConstType
    | TokenEOF
    deriving (Eq,Show)

-- Read everything then scan for testing
-- main = do
--     alexMonadScan

-- We rewrite alexMonadScan' to return the position when lexing fails (the
-- default implementation just returns an error message).
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (pos, _, _, _) -> alexError (show pos)
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len


-- Taken from dagit's happy-plus-alex example at https://github.com/dagit/happy-plus-alex
alexEOF = return TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> a) -> AlexAction a
lex f = \(_,_,_,s) i -> return (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: a -> AlexAction a
lex' = lex . const
}
