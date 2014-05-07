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
    const       { TokenConstType }
    id          { TokenId $$ }

%%

-- Program : Statements    { Program $1 }
--     p[0] = makeFamily('PROGRAM', p[1])
--     p.set_lineno(0, p.lineno(1))
-- 
-- # Returns a single statement node, which is the left-most sibling in its set
-- def p_statementso(p):
--     '''statements : statement
--                 | statement statements'''
--     if len(p) == 2:
--         # Luckily, p[1] is already a statement node
--         p[0] = p[1]
--     else:
--         # p[2] will be the head of its sibling list.
--         # So all we need to do is add it and its siblings to the right
--         # of p[1]
--         p[0] = p[1].makeSiblings(p[2])
--     p.set_lineno(0, p.lineno(1))
-- 
-- def p_statement(p):
--     '''statement : returnstmt SEMICOL
--                  | ifstmt
--                  | varassign SEMICOL
--                  | vardecl SEMICOL'''
--     # Alternation of semicolon-statements, so no node-association logic
--     p[0] = p[1]
--     p.set_lineno(0, p.lineno(1))
-- 
-- def p_returnstmt(p):
--     'returnstmt : RETURN expr'
--     p[0] = makeFamily('RETURN', p[2])
--     p.set_lineno(0, p.lineno(2))
-- 
-- def p_varassign(p):
--     'varassign : ID EQUALS expr'
--     p[0] = makeFamily('ASSIGN', makeNode(p[1]), p[3])
--     p.set_lineno(0, p.lineno(1))
-- 
-- def p_vardecl(p):
--     'vardecl : typeconstructor vars'
--     p[0] = makeFamily('DECLARESET', p[1], p[2])
--     p.set_lineno(0, p.lineno(1))
-- 
-- def p_ifstmt(p):
--     '''ifstmt : IF LPAREN boolexpr RPAREN LCURLY statements RCURLY
--               | IF LPAREN boolexpr RPAREN LCURLY statements RCURLY ELSE statement SEMICOL
--               | IF LPAREN boolexpr RPAREN LCURLY statements RCURLY ELSE LCURLY statements RCURLY'''
--     # print("Entering if with: ", p[1], p[2], p[3], p[4], p[5], p[6], p[7])
--     if len(p) > 8:
--         # Has an else
--         # The ternary-like part differentiates where the stmt/stmts come from
--         # p[0] = ('IF', p[3], 'THEN', p[6], 'ELSE', p[10] if len(p) > 11 else p[9])
--         p[0] = makeFamily('IF', p[3], p[6], p[10] if len(p) > 11 else p[9])
--     else:
--         # p[0] = ('IF', p[3], 'THEN', p[6])
--         p[0] = makeFamily('IF', p[3], p[6], makeNode())
--     # print("IFSTMT", p[0])
--     p.set_lineno(0, p.lineno(1))
-- 

-- TypeConstructor : TypeModifier TypeConstructor     { modifyType $1 $2 }
--                 | Type      { Type [$1] }
-- 
-- Type : "int"      { $1 }
-- 
-- TypeModifier : const    { $1 }

-- def p_vars(p):
--     '''vars : ID
--             | ID COMMA vars
--             | ID EQUALS expr
--             | ID EQUALS expr COMMA vars'''
--     if len(p) == 2:
--         # Just an ID
--         p[0] = makeFamily('DECLARE', makeNode(p[1]))
--     elif p[2] == '=':
--         # we are definitely assigning the ID
--         if len(p) > 4:
--             # Creates a new family alongside other families:
--             #   The new family is a DECLARE node strung with [ID -> VALUE]
--             p[0] = p[5].makeSiblings(makeFamily('DECLARE', makeNode(p[1]), p[3]))
--         else:
--             p[0] = makeFamily('DECLARE', makeNode(p[1]), p[3])
--     else:
--         p[0] = makeFamily('DECLARE', makeNode(p[1])).makeSiblings(p[3])
--     p.set_lineno(0, p.lineno(1))
-- 
-- def p_boolexpr(p):
--     '''boolexpr : expr LTHAN expr
--                 | expr GTHAN expr
--                 | expr EQUALSEQUALS expr
--                 | expr LTHAN EQUALS expr
--                 | expr GTHAN EQUALS expr
--                 | expr'''
--     if len(p) == 2:
--         p[0] = p[1]
--     elif len(p) == 4:
--         p[0] = makeFamily(p[2], p[1], p[3])
--     else:
--         p[0] = makeFamily(p[2]+p[3], p[1], p[4])
--     p.set_lineno(0, p.lineno(1))
-- 
-- def p_expr(p):
--     '''expr : term
--             | term PLUS term
--             | term MINUS term'''
--     if len(p) == 2:
--         p[0] = p[1]
--     else:
--         # Creates an oprator node that's strung with the terms to opreate on as its children
--         p[0] = makeFamily(p[2], p[1], p[3])
--     p.set_lineno(0, p.lineno(1))
-- 

Term : Part             { $1 }
     | Part "*" Part    { mkFamily TermMult [$1, $3] }
     | Part "/" Part    { mkFamily TermDiv  [$1, $3] }
     | Part "<<" Part   { mkFamily TermLSh  [$1, $3] }
     | Part ">>" Part   { mkFamily TermRSh  [$1, $3] }

Part : id       { PartID  $1 }
     | int      { PartNum $1 }
--     | LPAREN expr RPAREN'''

-- def find_column(input,token):
--     last_cr = input.rfind('\n',0,token.lexpos)
--     if last_cr < 0:
--         last_cr = -1
--     column = (token.lexpos - last_cr)
--     return column
-- 
-- def p_error(p):
--     if p is not None:
--         print("Syntax error at '%s', line %d, column %d" % (p.value, p.lineno, find_column(lexer.datainput,p)))
--     else:
--         print("Syntax error at Null. WTF")
-- 
-- yacc.yacc()
-- parseTree = yacc.parse(lexer.datainput)
-- 
-- # print("Parse Tree:")
-- # parseTree.prettyPrintStructure()
-- 
-- # print("Visitation tree:")
-- visitor.PrintVisitor().visit(parseTree)
-- 
-- # import pprint
-- # pp = pprint.PrettyPrinter(indent=4)
-- #pp.pprint(parseTree)
-- 

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

parseExp :: String -> Either String Term
parseExp s = runAlex s parse

readExp :: FilePath -> IO (Either String Term)
readExp fp = do
  cs <- readFile fp
  return (parseExp cs)

}
