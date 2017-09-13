-- This grammar file is based on the template found here:
-- https://github.com/dagit/happy-plus-alex

{
{-# OPTIONS -w #-}
module Parser( parseProg ) where

import Lexer
import qualified Ast
import Symtab (Id(..))
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
-- Without this we get a type error
%error { happyError }

%token
  true         { Token $$ (TokenBool True) }
  false        { Token $$ (TokenBool False) }
  natVal       { Token _ (TokenNat _) }
  if           { Token $$ TokenIf }
  else         { Token $$ TokenElse }
  while        { Token $$ TokenWhile }
  skip         { Token $$ TokenSkip }
  def          { Token $$ TokenDef }
  verify       { Token $$ TokenVerify }
  eval         { Token $$ TokenEval }
  intros       { Token $$ TokenIntros }
  assume       { Token $$ TokenAssume }
  assert       { Token $$ TokenAssert }
  bound        { Token $$ TokenBound }
  id           { Token _ (TokenId _) }
  ':='         { Token $$ TokenAssign }
  '=='         { Token $$ TokenEq }
  '<'          { Token $$ TokenLt }
  '<='         { Token $$ TokenLe }
  '+'          { Token $$ TokenPlus }
  '-'          { Token $$ TokenMinus }
  '*'          { Token $$ TokenMult }
  '|'          { Token $$ TokenOr }
  '&'          { Token $$ TokenAnd }
  '!'          { Token $$ TokenNot }
  '/'          { Token $$ TokenDiv }
  '('          { Token $$ TokenLParen }
  ')'          { Token $$ TokenRParen }
  '{'          { Token $$ TokenLBrace }
  '}'          { Token $$ TokenRBrace }
  ':'          { Token $$ TokenColon }
  ';'          { Token $$ TokenSemicolon }
  ','          { Token $$ TokenComma }
  -- eof          { Token $$ TokenEOF }

%right ';'
%nonassoc ':='
%nonassoc true false natVal id
%left APP
%left '+' '-'
%left '*' '/'
%nonassoc '(' ')' '{' '}'
%nonassoc '!' '<' '<=' '=='
%right '&'
%left '|'
%%

Prog :
  optlist(Funs) Verify Eval { Ast.Prog { Ast.funs_of = $1,
                                         Ast.verify_of = $2,      
                                         Ast.eval_of = $3 } }
AExp :
  natVal { case $1 of
             Token _ (TokenNat n) -> Ast.ANum n }
  | id { case $1 of
	   Token pos (TokenId id) -> Ast.AVar pos id }
  | id '(' optlist(AExps) ')'
    { case $1 of
        Token pos (TokenId id) -> Ast.ACall pos id $3 }
  | AExp '+' AExp { Ast.APlus $1 $3 }
  | AExp '-' AExp { Ast.AMinus $1 $3 }
  | AExp '*' AExp { Ast.AMult $1 $3 }
  | AExp '/' AExp { Ast.ADiv $1 $3 }
  | '(' AExp ')'  { $2 }
  | '-' AExp { Ast.AMinus (Ast.ANum 0) $2 }

AExps :
  AExp { [$1] }
  | AExps ',' AExp { $1 ++ [$3] }

BExp :
  true             { Ast.BTrue }
  | false          { Ast.BFalse }
  | AExp '<' AExp  { Ast.BLt $1 $3 }
  | AExp '<=' AExp { Ast.BLe $1 $3 }
  | AExp '==' AExp { Ast.BEq $1 $3 }
  | BExp '|' BExp  { Ast.BOr $1 $3 }
  | BExp '&' BExp  { Ast.BAnd $1 $3 }
  | '!' BExp       { Ast.BNot $2 }
  | '(' BExp ')'   { $2 }

Com :
  skip { Ast.CSkip }
  | id ':=' AExp { case $1 of
                     Token _ (TokenId id) -> Ast.CAss id $3 }
  | Com ';' Com { Ast.CSeq $1 $3 }
  | Com ';' { $1 }
  | if BExp '{' Com '}' else '{' Com '}' { Ast.CIf $2 $4 $8 }
  | while BExp '{' Com '}' { Ast.CWhile $2 $4 }

Fun :
  def id '(' optlist(Ids) ')' ':' id '{' Com '}'
    { case ($2, $7) of
        (Token _ (TokenId id1), Token _ (TokenId id2)) ->
          Ast.Fun { Ast.fid_of = id1,
		    Ast.params_of = $4,
		    Ast.out_of = id2,
		    Ast.body_of = $9 } }

Funs :
  Fun { [$1] }
  | Funs Fun { $1 ++ [$2] }

Ids :
  id { case $1 of
         Token _ (TokenId id) -> [id] }
  | Ids ',' id { case $3 of
                   Token _ (TokenId id) -> $1 ++ [id] }

VCom :
  skip { Ast.VCSkip }
  | intros Ids { Ast.VCIntros $2 }
  | assume BExp { Ast.VCAssume $2 }
  | assert BExp { Ast.VCAssert $2 }
  | bound natVal { case $2 of
                     Token _ (TokenNat n) -> Ast.VCBound n }
  | VCom ';' VCom { Ast.VCSeq $1 $3 }
  | VCom ';' { $1 }

Verify :
  verify '{' VCom '}' { $3 }

Eval :
  eval '{' AExp '}' { $3 }

opt(p) :
  p { Just $1 }
  | { Nothing }

optlist(p) :
  p { $1 }
  | { [] }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseProg :: FilePath -> String -> Either String Ast.Prog
parseProg = runAlex' parse
}
