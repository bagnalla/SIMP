-- This module defines the internal language syntax for Simp.

module Ast (
  AExp(..), BExp(..), Com(..), Fun(..), Prog(..), VCom(..)
  ) where

import Lexer (AlexPosn)
import Data.List (intercalate)
import Symtab (Id(..))

-------------------------
-- Arithmetic expressions

data AExp =
  ANum Integer
  | AVar AlexPosn Id
  | ACall AlexPosn Id [AExp]
  | APlus  AExp AExp
  | AMinus AExp AExp
  | AMult  AExp AExp
  | ADiv   AExp AExp
  deriving (Show, Eq)

----------------------
-- Boolean expressions

data BExp =
  BTrue
  | BFalse
  | BLt  AExp AExp
  | BLe  AExp AExp
  | BEq  AExp AExp
  | BOr  BExp BExp
  | BAnd BExp BExp
  | BNot BExp
  deriving (Show, Eq)

-----------
-- Commands

data Com =
  CSkip
  | CAss Id AExp
  | CSeq Com Com
  | CIf BExp Com Com
  | CWhile BExp Com
  deriving (Show, Eq)

------------------------
-- Verification commands

data VCom =
  VCSkip
  | VCIntros [Id]
  | VCAssume BExp
  | VCAssert BExp
  | VCBound Integer
  | VCSeq VCom VCom
  deriving (Show, Eq)

-----------------------
-- Function definitions

data Fun =
  Fun { fid_of    :: Id,
        params_of :: [Id],
        out_of    :: Id,
        body_of   :: Com }
  deriving (Show, Eq)

----------
-- Program

data Prog =
  Prog { funs_of   :: [Fun],
         verify_of :: VCom,
         eval_of   :: AExp }
  deriving (Show, Eq)
