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

module Language.Haskell.Exts.Syntax.Type where

import Prelude hiding (id)

import Data.Data
import GHC.Generics (Generic)
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif

import Language.Haskell.Exts.Syntax.Names
import Language.Haskell.Exts.Syntax.Boxed
import Language.Haskell.Exts.Syntax.Splice

-- | A type variable declaration, optionally with an explicit kind annotation.
data TyVarBind_ exp l
    = KindedVar   l (Name l) (Kind_ exp l)  -- ^ variable binding with kind annotation
    | UnkindedVar l (Name l)           -- ^ ordinary variable binding
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An explicit kind annotation.
type Kind_ exp = Type_ exp

-- | A context is a set of assertions
data Context_ exp l
    = CxSingle l (Asst_ exp l)
    | CxTuple  l [Asst_ exp l]
    | CxEmpty  l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Class assertions.
--   In Haskell 98, the argument would be a /tyvar/, but this definition
--   allows multiple parameters, and allows them to be /type/s.
--   Also extended with support for implicit parameters and equality constraints.
data Asst_ exp l
        = ClassA l (QName l) [Type_ exp l]           -- ^ ordinary class assertion
        | AppA l (Name l) [Type_ exp l]              -- ^ constraint kind assertion, @Dict :: cxt a => Dict cxt@
        | InfixA l (Type_ exp l) (QName l) (Type_ exp l)  -- ^ class assertion where the class name is given infix
        | IParam l (IPName l) (Type_ exp l)          -- ^ implicit parameter assertion
        | EqualP l (Type_ exp l) (Type_ exp l)            -- ^ type equality constraint
        | ParenA l (Asst_ exp l)                     -- ^ parenthesised class assertion
        | WildCardA l (Maybe (Name l))          -- ^ Context Wildcard
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | The type of a constructor argument or field, optionally including
--   a strictness annotation.
data BangType l
     = BangedTy   l -- ^ strict component, marked with \"@!@\"
     | LazyTy     l -- ^ lazy component, marked with \"@~@\"
     | NoStrictAnnot l -- ^ No strictness information
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

data Unpackedness l
    = Unpack l -- ^ \"@{-\# UNPACK \#-}@\"
    | NoUnpack l -- ^ \"@{-\# NOUNPACK \#-}@\"
    | NoUnpackPragma l -- ^ No unpack pragma
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A type qualified with a context.
--   An unqualified type has an empty context.
data Type_ exp l
     = TyForall l
        (Maybe [TyVarBind_ exp l])
        (Maybe (Context_ exp l))
        (Type_ exp l)                                -- ^ qualified type
     | TyStar  l                                -- ^ @*@, the type of types
     | TyFun   l (Type_ exp l) (Type_ exp l)              -- ^ function type
     | TyTuple l Boxed [Type_ exp l]                 -- ^ tuple type, possibly boxed
     | TyUnboxedSum l [Type_ exp l]                  -- ^ unboxed tuple type
     | TyList  l (Type_ exp l)                       -- ^ list syntax, e.g. [a], as opposed to [] a
     | TyParArray  l (Type_ exp l)                   -- ^ parallel array syntax, e.g. [:a:]
     | TyApp   l (Type_ exp l) (Type_ exp l)              -- ^ application of a type constructor
     | TyVar   l (Name l)                       -- ^ type variable
     | TyCon   l (QName l)                      -- ^ named type or type constructor
     | TyParen l (Type_ exp l)                       -- ^ type surrounded by parentheses
     | TyInfix l (Type_ exp l) (MaybePromotedName l)
                          (Type_ exp l)              -- ^ infix type constructor
     | TyKind  l (Type_ exp l) (Kind_ exp l)              -- ^ type with explicit kind signature
     | TyPromoted l (Promoted_ exp l)                -- ^ @'K@, a promoted data type (-XDataKinds).
     | TyEquals l (Type_ exp l) (Type_ exp l)             -- ^ type equality predicate enabled by ConstraintKinds
     | TySplice l (Splice_ exp l)                    -- ^ template haskell splice type
     | TyBang l (BangType l) (Unpackedness l) (Type_ exp l)           -- ^ Strict type marked with \"@!@\" or type marked with UNPACK pragma.
     | TyWildCard l (Maybe (Name l))            -- ^ Either an anonymous of named type wildcard
     | TyQuasiQuote l String String             -- ^ @[$/name/| /string/ |]@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Bools here are True if there was a leading quote which may be
-- left out. For example @'[k1,k2]@ means the same thing as @[k1,k2]@.
data Promoted_ exp l
        = PromotedInteger l Integer String -- ^ parsed value and raw string
        | PromotedString l String String -- ^ parsed value and raw string
        | PromotedCon l Bool (QName l)
        | PromotedList l Bool [Type_ exp l]
        | PromotedTuple l [Type_ exp l]
        | PromotedUnit l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)
