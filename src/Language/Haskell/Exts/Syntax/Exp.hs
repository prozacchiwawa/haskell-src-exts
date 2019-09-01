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

module Language.Haskell.Exts.Syntax.Exp where

import Prelude hiding (id)

import Data.Data
import GHC.Generics (Generic)
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif

import Language.Haskell.Exts.Syntax.Names
import Language.Haskell.Exts.Syntax.Literal
import Language.Haskell.Exts.Syntax.Pattern
import Language.Haskell.Exts.Syntax.Stmt
import Language.Haskell.Exts.Syntax.Binds
import Language.Haskell.Exts.Syntax.Boxed
import Language.Haskell.Exts.Syntax.Type
import Language.Haskell.Exts.Syntax.Splice
import Language.Haskell.Exts.Syntax.XAttr
import Language.Haskell.Exts.Syntax.XName

type Stmt_ decl = Stmt___ (Pat__ (Exp_ decl) decl) (Exp_ decl) decl
type QualStmt_ decl = QualStmt___ (Pat__ (Exp_ decl) decl) (Exp_ decl) decl
type Binds_ decl = Binds__ (Exp_ decl) decl

-- | Possibly qualified infix operators (/qop/), appearing in expressions.
data QOp l
    = QVarOp l (QName l) -- ^ variable operator (/qvarop/)
    | QConOp l (QName l) -- ^ constructor operator (/qconop/)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Operators appearing in @infix@ declarations are never qualified.
data Op l
    = VarOp l (Name l)    -- ^ variable operator (/varop/)
    | ConOp l (Name l)    -- ^ constructor operator (/conop/)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Haskell expressions.
data Exp_ decl l
    = Var l (QName l)                       -- ^ variable
    | OverloadedLabel l String              -- ^ Overloaded label #foo
    | IPVar l (IPName l)                    -- ^ implicit parameter variable
    | Con l (QName l)                       -- ^ data constructor
    | Lit l (Literal l)                     -- ^ literal constant
    | InfixApp l (Exp_ decl l) (QOp l) (Exp_ decl l)    -- ^ infix application
    | App l (Exp_ decl l) (Exp_ decl l)                 -- ^ ordinary application
    | NegApp l (Exp_ decl l)                      -- ^ negation expression @-/exp/@ (unary minus)
    | Lambda l [Pat__ (Exp_ decl) decl l] (Exp_ decl l)              -- ^ lambda expression
    | Let l (Binds_ decl l) (Exp_ decl l)               -- ^ local declarations with @let@ ... @in@ ...
    | If l (Exp_ decl l) (Exp_ decl l) (Exp_ decl l)          -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
    | MultiIf l [GuardedRhs_ decl l]              -- ^ @if@ @|@ /stmts/ @->@ /exp/ ...
    | Case l (Exp_ decl l) [Alt_ decl l]                -- ^ @case@ /exp/ @of@ /alts/
    | Do l [Stmt_ decl l]                         -- ^ @do@-expression:
                                            --   the last statement in the list
                                            --   should be an expression.
    | MDo l [Stmt_ decl l]                        -- ^ @mdo@-expression
    | Tuple l Boxed [Exp_ decl l]                 -- ^ tuple expression
    | UnboxedSum l Int Int (Exp_ decl l)          -- ^ unboxed sum
    | TupleSection l Boxed [Maybe (Exp_ decl l)]  -- ^ tuple section expression, e.g. @(,,3)@
    | List l [Exp_ decl l]                        -- ^ list expression
    | ParArray l [Exp_ decl l]                    -- ^ parallel array expression
    | Paren l (Exp_ decl l)                       -- ^ parenthesised expression
    | LeftSection l (Exp_ decl l) (QOp l)         -- ^ left section @(@/exp/ /qop/@)@
    | RightSection l (QOp l) (Exp_ decl l)        -- ^ right section @(@/qop/ /exp/@)@
    | RecConstr l (QName l) [FieldUpdate_ decl l] -- ^ record construction expression
    | RecUpdate l (Exp_ decl l)   [FieldUpdate_ decl l] -- ^ record update expression
    | EnumFrom l (Exp_ decl l)                    -- ^ unbounded arithmetic sequence,
                                            --   incrementing by 1: @[from ..]@
    | EnumFromTo l (Exp_ decl l) (Exp_ decl l)          -- ^ bounded arithmetic sequence,
                                            --   incrementing by 1 @[from .. to]@
    | EnumFromThen l (Exp_ decl l) (Exp_ decl l)        -- ^ unbounded arithmetic sequence,
                                            --   with first two elements given @[from, then ..]@
    | EnumFromThenTo l (Exp_ decl l) (Exp_ decl l) (Exp_ decl l)
                                            -- ^ bounded arithmetic sequence,
                                            --   with first two elements given @[from, then .. to]@
    | ParArrayFromTo l (Exp_ decl l) (Exp_ decl l)      -- ^ Parallel array bounded arithmetic sequence,
                                            --   incrementing by 1 @[:from .. to:]@
    | ParArrayFromThenTo l (Exp_ decl l) (Exp_ decl l) (Exp_ decl l)
                                            -- ^ bounded arithmetic sequence,
                                            --   with first two elements given @[:from, then .. to:]@
    | ListComp l (Exp_ decl l) [QualStmt_ decl l]       -- ^ ordinary list comprehension
    | ParComp  l (Exp_ decl l) [[QualStmt_ decl l]]     -- ^ parallel list comprehension
    | ParArrayComp  l (Exp_ decl l) [[QualStmt_ decl l]] -- ^ parallel array comprehension
    | ExpTypeSig l (Exp_ decl l) (Type_ (Exp_ decl) l)         -- ^ expression with explicit type signature

    | VarQuote l (QName l)                  -- ^ @'x@ for template haskell reifying of expressions
    | TypQuote l (QName l)                  -- ^ @''T@ for template haskell reifying of types
    | BracketExp l (Bracket_ decl l)              -- ^ template haskell bracket expression
    | SpliceExp l (Splice_ (Exp_ decl) l)                -- ^ template haskell splice expression
    | QuasiQuote l String String            -- ^ quasi-quotaion: @[$/name/| /string/ |]@
    | TypeApp l (Type_ (Exp_ decl) l)                    -- ^ Visible type application

-- Hsx
    | XTag l (XName l) [XAttr_ (Exp_ decl) l] (Maybe (Exp_ decl l)) [Exp_ decl l]
                                            -- ^ xml element, with attributes and children
    | XETag l (XName l) [XAttr_ (Exp_ decl) l] (Maybe (Exp_ decl l))
                                            -- ^ empty xml element, with attributes
    | XPcdata l String                      -- ^ PCDATA child element
    | XExpTag l (Exp_ decl l)                     -- ^ escaped haskell expression inside xml
    | XChildTag l [Exp_ decl l]                   -- ^ children of an xml element


-- Pragmas
    | CorePragma l      String (Exp_ decl l)      -- ^ CORE pragma
    | SCCPragma  l      String (Exp_ decl l)      -- ^ SCC pragma
    | GenPragma  l      String (Int, Int) (Int, Int) (Exp_ decl l)
                                            -- ^ GENERATED pragma

-- Arrows
    | Proc            l (Pat__ (Exp_ decl) decl l) (Exp_ decl l)     -- ^ arrows proc: @proc@ /pat/ @->@ /exp/
    | LeftArrApp      l (Exp_ decl l) (Exp_ decl l)     -- ^ arrow application (from left): /exp/ @-<@ /exp/
    | RightArrApp     l (Exp_ decl l) (Exp_ decl l)     -- ^ arrow application (from right): /exp/ @>-@ /exp/
    | LeftArrHighApp  l (Exp_ decl l) (Exp_ decl l)     -- ^ higher-order arrow application (from left): /exp/ @-<<@ /exp/
    | RightArrHighApp l (Exp_ decl l) (Exp_ decl l)     -- ^ higher-order arrow application (from right): /exp/ @>>-@ /exp/

-- LambdaCase
    | LCase l [Alt_ decl l]                       -- ^ @\case@ /alts/

  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | The right hand side of a function binding, pattern binding, or a case
--   alternative.
data Rhs_ decl l
     = UnGuardedRhs l (Exp_ decl l) -- ^ unguarded right hand side (/exp/)
     | GuardedRhss  l [GuardedRhs_ decl l]
                -- ^ guarded right hand side (/gdrhs/)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A guarded right hand side @|@ /stmts/ @=@ /exp/, or @|@ /stmts/ @->@ /exp/
--   for case alternatives.
--   The guard is a series of statements when using pattern guards,
--   otherwise it will be a single qualifier expression.
data GuardedRhs_ decl l
     = GuardedRhs l [Stmt_ decl l] (Exp_ decl l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An /alt/ alternative in a @case@ expression.
data Alt_ decl l
    = Alt l (Pat__ (Exp_ decl) decl l) (Rhs_ decl l) (Maybe (Binds_ decl l))
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An /fbind/ in a labeled construction or update expression.
data FieldUpdate_ decl l
    = FieldUpdate l (QName l) (Exp_ decl l)    -- ^ ordinary label-expresion pair
    | FieldPun l (QName l)               -- ^ record field pun
    | FieldWildcard l                    -- ^ record field wildcard
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A template haskell bracket expression.
data Bracket_ decl l
    = ExpBracket l (Exp_ decl l)        -- ^ expression bracket: @[| ... |]@
    | PatBracket l (Pat__ (Exp_ decl) decl l)        -- ^ pattern bracket: @[p| ... |]@
    | TypeBracket l (Type_ (Exp_ decl) l)      -- ^ type bracket: @[t| ... |]@
    | DeclBracket l [decl l]      -- ^ declaration bracket: @[d| ... |]@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

