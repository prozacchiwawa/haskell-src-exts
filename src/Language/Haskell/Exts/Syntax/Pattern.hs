{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Syntax
-- Copyright   :  (c) Niklas Broberg 2004-2009,
--                (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- A suite of datatypes describing the (semi-concrete) abstract syntax of Haskell 98
-- <http://www.haskell.org/onlinereport/> plus registered extensions, including:
--
--   * multi-parameter type classes with functional dependencies (MultiParamTypeClasses, FunctionalDependencies)
--
--   * parameters of type class assertions are unrestricted (FlexibleContexts)
--
--   * 'forall' types as universal and existential quantification (RankNTypes, ExistentialQuantification, etc)
--
--   * pattern guards (PatternGuards)
--
--   * implicit parameters (ImplicitParameters)
--
--   * generalised algebraic data types (GADTs)
--
--   * template haskell (TemplateHaskell)
--
--   * empty data type declarations (EmptyDataDecls)
--
--   * unboxed tuples (UnboxedTuples)
--
--   * regular patterns (RegularPatterns)
--
--   * HSP-style XML expressions and patterns (XmlSyntax)
--
-- All nodes in the syntax tree are annotated with something of a user-definable data type.
-- When parsing, this annotation will contain information about the source location that the
-- particular node comes from.
--
-----------------------------------------------------------------------------

module Language.Haskell.Exts.Syntax.Pattern where

import Prelude hiding (id)

import Data.Data
import GHC.Generics (Generic)
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif

import Language.Haskell.Exts.Syntax.Names
import Language.Haskell.Exts.Syntax.XName
import Language.Haskell.Exts.Syntax.PXAttr
import Language.Haskell.Exts.Syntax.Sign
import Language.Haskell.Exts.Syntax.Literal
import Language.Haskell.Exts.Syntax.Boxed
import Language.Haskell.Exts.Syntax.Stmt
import Language.Haskell.Exts.Syntax.Type
import Language.Haskell.Exts.Syntax.Splice

type PXAttr__ exp decl l = PXAttr___ (Pat__ exp decl) l
type Stmt__ exp decl = Stmt___ (Pat__ exp decl) exp decl

-- | A regular pattern operator.
data RPatOp l
    = RPStar  l  -- ^ @*@ = 0 or more
    | RPStarG l  -- ^ @*!@ = 0 or more, greedy
    | RPPlus  l  -- ^ @+@ = 1 or more
    | RPPlusG l  -- ^ @+!@ = 1 or more, greedy
    | RPOpt   l  -- ^ @?@ = 0 or 1
    | RPOptG  l  -- ^ @?!@ = 0 or 1, greedy
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An entity in a regular pattern.
data RPat__ exp decl l
    = RPOp l (RPat__ exp decl l) (RPatOp l)   -- ^ operator pattern, e.g. pat*
    | RPEither l (RPat__ exp decl l) (RPat__ exp decl l) -- ^ choice pattern, e.g. (1 | 2)
    | RPSeq l [RPat__ exp decl l]             -- ^ sequence pattern, e.g. (| 1, 2, 3 |)
    | RPGuard l (Pat__ exp decl l) [Stmt__ exp decl l]   -- ^ guarded pattern, e.g. (| p | p < 3 |)
    | RPCAs l (Name l) (RPat__ exp decl l)    -- ^ non-linear variable binding, e.g. (foo\@:(1 | 2))*
    | RPAs l (Name l) (RPat__ exp decl l)     -- ^ linear variable binding, e.g. foo\@(1 | 2)
    | RPParen l (RPat__ exp decl l)           -- ^ parenthesised pattern, e.g. (2*)
    | RPPat l (Pat__ exp decl l)              -- ^ an ordinary pattern
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An /fpat/ in a labeled record pattern.
data PatField__ exp decl l
    = PFieldPat l (QName l) (Pat__ exp decl l)     -- ^ ordinary label-pattern pair
    | PFieldPun l (QName l)             -- ^ record field pun
    | PFieldWildcard l                  -- ^ record field wildcard
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A pattern, to be matched against a value.
data Pat__ exp decl l
    = PVar l (Name l)                       -- ^ variable
    | PLit l (Sign l) (Literal l)           -- ^ literal constant
    | PNPlusK l (Name l) Integer            -- ^ n+k pattern
    | PInfixApp l (Pat__ exp decl l) (QName l) (Pat__ exp decl l) -- ^ pattern with an infix data constructor
    | PApp l (QName l) [Pat__ exp decl l]              -- ^ data constructor and argument patterns
    | PTuple l Boxed [Pat__ exp decl l]                -- ^ tuple pattern
    | PUnboxedSum l Int Int (Pat__ exp decl l)         -- ^ unboxed sum
    | PList l [Pat__ exp decl l]                       -- ^ list pattern
    | PParen l (Pat__ exp decl l)                      -- ^ parenthesized pattern
    | PRec l (QName l) [PatField__ exp decl l]         -- ^ labelled pattern, record style
    | PAsPat l (Name l) (Pat__ exp decl l)             -- ^ @\@@-pattern
    | PWildCard l                           -- ^ wildcard pattern: @_@
    | PIrrPat l (Pat__ exp decl l)                     -- ^ irrefutable pattern: @~/pat/@
    | PatTypeSig l (Pat__ exp decl l) (Type_ exp l)         -- ^ pattern with type signature
    | PViewPat l (exp l) (Pat__ exp decl l)            -- ^ view patterns of the form @(/exp/ -> /pat/)@
    | PRPat l [RPat__ exp decl l]                      -- ^ regular list pattern
    | PXTag l (XName l) [PXAttr__ exp decl l] (Maybe (Pat__ exp decl l)) [Pat__ exp decl l]
                                            -- ^ XML element pattern
    | PXETag l (XName l) [PXAttr__ exp decl l] (Maybe (Pat__ exp decl l))
                                            -- ^ XML singleton element pattern
    | PXPcdata l String                     -- ^ XML PCDATA pattern
    | PXPatTag l (Pat__ exp decl l)                    -- ^ XML embedded pattern
    | PXRPats  l [RPat__ exp decl l]                   -- ^ XML regular list pattern
    | PSplice l (Splice_ exp l)                  -- ^ template haskell splice pattern
    | PQuasiQuote l String String           -- ^ quasi quote pattern: @[$/name/| /string/ |]@
    | PBangPat l (Pat__ exp decl l)                    -- ^ strict (bang) pattern: @f !x = ...@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)
