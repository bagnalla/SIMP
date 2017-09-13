-- This file is heavily based on the template from
-- https://github.com/dagit/happy-plus-alex

{
{-# OPTIONS -w  #-}
module Lexer
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , unLex
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  ) where
import Prelude hiding (lex)
import Data.Char (chr)
import Control.Monad ( liftM )
import Symtab (Id(..))
}
%wrapper "monadUserState"
$digit = 0-9
$alpha = [A-Za-z]
tokens :-

  "#".*                         ;
  $white+                       ;
  true                          { lex' $ TokenBool True }
  false                         { lex' $ TokenBool False }
  if                            { lex' TokenIf }
  else                          { lex' TokenElse }
  while                         { lex' TokenWhile }
  SKIP                          { lex' TokenSkip }
  $digit+                       { lex (TokenNat . read) }
  \(                            { lex' TokenLParen }
  \)                            { lex' TokenRParen }
  "{"                           { lex' TokenLBrace }
  "}"                           { lex' TokenRBrace }
  ":="                          { lex' TokenAssign }
  \:                            { lex' TokenColon }
  \;                            { lex' TokenSemicolon }
  \,                            { lex' TokenComma }
  "+"                           { lex' TokenPlus }
  "-"                           { lex' TokenMinus }
  "*"                           { lex' TokenMult }
  "/"                           { lex' TokenDiv }
  "=="                          { lex' TokenEq }
  "<"                           { lex' TokenLt }
  "<="                          { lex' TokenLe }
  "||"                          { lex' TokenOr }
  "&&"                          { lex' TokenAnd }
  "!"                           { lex' TokenNot }
  "def"                         { lex' TokenDef }
  "verify"                      { lex' TokenVerify }
  "eval"                        { lex' TokenEval }
  "intros"                      { lex' TokenIntros }
  "assume"                      { lex' TokenAssume }
  "assert"                      { lex' TokenAssert }
  "bound"                       { lex' TokenBound }
  eof                        { lex' TokenEOF }
  $alpha [$alpha $digit \_ \']* { lex (TokenId . Id) }

{
-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving ( Show )

data TokenClass =
  TokenId Id
  | TokenLParen
  | TokenRParen
  | TokenLBrace
  | TokenRBrace
  | TokenAssign
  | TokenColon
  | TokenSemicolon
  | TokenComma
  | TokenBool Bool
  | TokenNat Integer
  | TokenEq
  | TokenIf
  | TokenElse
  | TokenWhile
  | TokenPlus
  | TokenMinus
  | TokenMult
  | TokenDiv
  | TokenLt
  | TokenLe
  | TokenOr
  | TokenAnd
  | TokenNot
  | TokenSkip
  | TokenDef
  | TokenVerify
  | TokenEval
  | TokenIntros
  | TokenAssume
  | TokenAssert
  | TokenBound
  | TokenEOF
    deriving (Eq,Show)

-- For nice parser error messages.
unLex :: TokenClass -> String
unLex (TokenId id)         = show id
unLex TokenLParen          = "("
unLex TokenRParen          = ")"
unLex TokenLBrace          = "{"
unLex TokenRBrace          = "}"
unLex TokenAssign          = ":="
unLex TokenColon           = ":"
unLex TokenSemicolon       = ";"
unLex TokenComma           = ","
unLex (TokenBool b)        = show b
unLex (TokenNat i)         = show i
unLex TokenEq              = "=="
unLex TokenIf              = "if"
unLex TokenElse            = "else"
unLex TokenWhile           = "while"
unLex TokenPlus            = "+"
unLex TokenMinus           = "-"
unLex TokenMult            = "*"
unLex TokenDiv             = "/"
unLex TokenLt              = "<"
unLex TokenLe              = "<="
unLex TokenOr              = "||"
unLex TokenAnd             = "&&"
unLex TokenNot             = "!"
unLex TokenSkip            = "SKIP"
unLex TokenDef             = "def"
unLex TokenVerify          = "verify"
unLex TokenEval            = "eval"
unLex TokenIntros          = "intros"
unLex TokenAssume          = "assume"
unLex TokenAssert          = "assert"
unLex TokenBound           = "bound"
unLex TokenEOF             = "<EOF>"

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}
