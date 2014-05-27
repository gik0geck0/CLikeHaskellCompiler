{
{-# OPTIONS -w #-}
module CLikeParser( parseExp, readExp ) where

import CLikeLexer
import CLikeTypes

}

-- Much of the boiler plate for this came from dagit's example at https://github.com/dagit/happy-plus-alex

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { TokenEOF }
%error { happyError }

%token
    int         { TokenInt $$ }
    "+"         { TokenPlus }
    "-"         { TokenMinus }
    "*"         { TokenTimes }
    "/"         { TokenDivide }
    "%"         { TokenModulus }
    "<<"        { TokenLShift }
    ">>"        { TokenRShift }
    "=="        { TokenEqualsEquals }
    "="         { TokenEquals }
    "<"         { TokenLThan }
    ">"         { TokenGThan }
    "<="        { TokenLEThan }
    ">="        { TokenGEThan }
    "("         { TokenLParen }
    ")"         { TokenRParen }
    "{"         { TokenLCurly }
    "}"         { TokenRCurly }
    ","         { TokenComma }
    ";"         { TokenSemicol }
    if          { TokenIf }
    else        { TokenElse }
    return      { TokenReturn }
    "int"       { TokenIntType }
    "const"     { TokenConstType }
    id          { TokenId $$ }

%%

Program : Statements    { mkFamily CompilationUnit [$1] }
Statements : Statement  { mkFamily Statements [$1] }
           | Statement Statements   { adoptChildren (mkNode Statements Nothing) (getNodeChildren $2) }

Statement : VarDecl ";"     { $1 }
          | ReturnStmt ";"  { $1 }
--          | IfStmt          { $1 }
          | VarAssign ";"   { $1 }

ReturnStmt : return Expr  { mkFamily ReturnStatement [$2] }

VarAssign : Identifier "=" Expr     { mkFamily VariableAssignStatement [$1,$3] }

-- def p_ifstmt(p):
--     '''ifstmt : IF LPAREN boolexpr RPAREN LCURLY statements RCURLY
--               | IF LPAREN boolexpr RPAREN LCURLY statements RCURLY ELSE statement SEMICOL
--               | IF LPAREN boolexpr RPAREN LCURLY statements RCURLY ELSE LCURLY statements RCURLY'''
--     # print("Entering if with: ", p[1], p[2], p[3], p[4], p[5], p[6], p[7])
--     if len(p) > 8:
--         # Has an else
--         # The ternary-like part differentiates where the stmt/stmts come from . read
--         # p[0] = ('IF', p[3], 'THEN', p[6], 'ELSE', p[10] if len(p) > 11 else p[9])
--         p[0] = makeFamily('IF', p[3], p[6], p[10] if len(p) > 11 else p[9])
--     else:
--         # p[0] = ('IF', p[3], 'THEN', p[6])
--         p[0] = makeFamily('IF', p[3], p[6], makeNode())
--     # print("IFSTMT", p[0])
--     p.set_lineno(0, p.lineno(1))
-- 

VarDecl : TypeConstructor Vars  { mkFamily DeclarationStatement [$1,$2] }

TypeConstructor : TypeModifier TypeConstructor     { mkFamily TypeConstructor [$1,$2] }
                | Type      { $1 }

Type : "int"    { mkNode Type $ Just $ StringData "int" }

TypeModifier : "const"      { mkNode TypeModifier $ Just $ StringData "const" }
             | TypeModifier TypeModifier { mkFamily TypeModifierMulti [$1,$2] }

Vars : Identifier           { mkFamily DeclareVariable [$1] }
     | Identifier "," Vars          { mkFamily DeclareVariableMulti [$1, $3] }
     | Identifier "=" Expr          { mkFamily InstantiateVariable [$1,$3] }
     | Identifier "=" Expr "," Vars { mkFamily InstantiateVariableMulti [$1,$3,$5] }

BoolExpr : Expr "<" Expr    { mkFamily BooleanOperationLThan [$1, $3] }
         | Expr ">" Expr    { mkFamily BooleanOperationGThan [$1, $3] }
         | Expr "==" Expr   { mkFamily BooleanOperationDoubleEquals [$1, $3] }
         | Expr "<=" Expr   { mkFamily BooleanOperationLEThan [$1, $3] }
         | Expr ">=" Expr   { mkFamily BooleanOperationGEThan [$1, $3] }

Expr : Term             { $1 }
     | BoolExpr         { $1 }
     | Expr "+" Expr    { mkFamily OperationPlus [$1, $3] }
     | Expr "-" Expr    { mkFamily OperationMinus [$1, $3] }

Term : Part             { $1 }
     | Expr "*" Expr    { mkFamily OperationMult [$1, $3] }
     | Expr "/" Expr    { mkFamily OperationDiv  [$1, $3] }
     | Expr "<<" Expr   { mkFamily OperationLSh  [$1, $3] }
     | Expr ">>" Expr   { mkFamily OperationRSh  [$1, $3] }

Part : id           { mkNode Identifier $ Just $ StringData $1 }
     | int          { mkNode Number $ Just $ IntegerData $1 }
     | "(" Expr ")" { $2 }

Identifier : id { mkNode Identifier $ Just $ StringData $1 }

{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap cont = do
    t <- alexMonadScan'
    cont t

-- We rewrite alexMonadScan' to return the position when lexing fails (the
-- default implementation just returns an error message).
-- alexMonadScan' = do
--   inp <- alexGetInput
--   sc <- alexGetStartCode
--   case alexScan inp sc of
--     AlexEOF -> alexEOF
--     AlexError (pos, _, _, _) -> alexError (show pos)
--     AlexSkip  inp' len -> do
--         alexSetInput inp'
--         alexMonadScan'
--     AlexToken inp' len action -> do
--         alexSetInput inp'
--         action (ignorePendingBytes inp) len

getPosn :: Alex (Int,Int)
getPosn = do
  (AlexPn _ l c,_,_,_) <- alexGetInput
  return (l,c)

happyError :: Token -> Alex a
happyError t = do
  (l,c) <- getPosn
  fail (show l ++ ":" ++ show c ++ ": Parse error on Token: " ++ show t ++ "\n")

parseExp :: String -> Either String Node
parseExp s = runAlex s parse

readExp :: FilePath -> IO (Either String Node)
readExp fp = do
  cs <- readFile fp
  return (parseExp cs)

}
