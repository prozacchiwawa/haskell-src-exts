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

module Language.Haskell.Exts.Syntax.Decl
  ( Exp(..)
  , Rhs(..)
  , GuardedRhs(..)
  , Alt(..)
  , FieldUpdate(..)
  , Bracket(..)

  , Pat(..)
  , RPat(..)
  , PatField(..)

  , XAttr(..)
  , TyVarBind(..)
  , Kind(..)
  , Context(..)
  , Asst(..)
  , Type(..)
  , Binds(..)
  , IPBind(..)
  , Promoted(..)

  , Stmt(..)
  , QualStmt(..)

  , PXAttr(..)

  , Splice(..)

  , Deriving(..)
  , DerivStrategy(..)
  , DeclHead(..)
  , Decl(..)
  , ResultSig(..)
  , InjectivityInfo(..)
  , TypeEqn(..)
  , DataOrNew(..)
  , QualConDecl(..)
  , ConDecl(..)
  , FieldDecl(..)
  , InstRule(..)
  , InstHead(..)
  , GadtDecl(..)
  , ClassDecl(..)
  , FunDep(..)
  , Overlap(..)
  , InstDecl(..)
  , Match(..)
  , CallConv(..)
  , Safety(..)
  , Annotation(..)
  , BooleanFormula(..)
  , Role(..)
  , Rule(..)
  , RuleVar(..)
  , Activation(..)
  , PatternSynDirection(..)
  )
where

import Prelude hiding (id)

import Data.Data
import GHC.Generics (Generic)
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif

import Language.Haskell.Exts.Syntax.XAttr
import Language.Haskell.Exts.Syntax.PXAttr
import Language.Haskell.Exts.Syntax.Pattern
import Language.Haskell.Exts.Syntax.Exp
import Language.Haskell.Exts.Syntax.Names
import Language.Haskell.Exts.Syntax.Type
import Language.Haskell.Exts.Syntax.Binds
import Language.Haskell.Exts.Syntax.Splice
import Language.Haskell.Exts.Syntax.Assoc
import Language.Haskell.Exts.Syntax.Stmt

-- Exp
type Exp = Exp_ Decl
type Rhs = Rhs_ Decl
type GuardedRhs = GuardedRhs_ Decl
type Alt = Alt_ Decl
type FieldUpdate = FieldUpdate_ Decl
type Bracket = Bracket_ Decl

-- Pattern
type Pat = Pat__ Exp Decl
type RPat = RPat__ Exp Decl
type PatField = PatField__ Exp Decl

-- XAttr
type XAttr = XAttr_ Exp

-- Type
type TyVarBind = TyVarBind_ Exp
type Kind = Type
type Context = Context_ Exp
type Asst = Asst_ Exp
type Type = Type_ Exp
type Binds = Binds__ Exp Decl
type IPBind = IPBind_ Exp
type Promoted = Promoted_ Exp

-- Stmt
type Stmt = Stmt___ Pat Exp Decl
type QualStmt = QualStmt___ Pat Exp Decl

-- PXAttr
type PXAttr = PXAttr___ Pat

-- Splice
type Splice = Splice_ Exp

-- | A deriving clause following a data type declaration.
data Deriving l = Deriving l (Maybe (DerivStrategy l)) [InstRule l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Which technique the user explicitly requested when deriving an instance.
data DerivStrategy l
  = DerivStock l        -- ^ GHC's \"standard\" strategy, which is to implement a
                        --   custom instance for the data type. This only works for
                        --   certain types that GHC knows about (e.g., 'Eq', 'Show',
                        --   'Functor' when @-XDeriveFunctor@ is enabled, etc.)
  | DerivAnyclass l     -- ^ @-XDeriveAnyClass@
  | DerivNewtype l      -- ^ @-XGeneralizedNewtypeDeriving@
  | DerivVia l (Type l) -- ^ @-XDerivingVia@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | The head of a type or class declaration, which consists of the type
-- or class name applied to some type variables
--
-- @class C a b@ is represented as
--
-- >DHApp
-- >   ()
-- >   (DHApp
-- >      () (DHead () (Ident () "C")) (UnkindedVar () (Ident () "a")))
-- >   (UnkindedVar () (Ident () "b"))
--
-- (where the annotation type @l@ is instantiated with @()@)
--
-- @class (a :< b) c@ is represented as
--
-- >DHApp
-- >   ()
-- >   (DHParen
-- >      ()
-- >      (DHApp
-- >         ()
-- >         (DHInfix () (UnkindedVar () (Ident () "a")) (Symbol () ":<"))
-- >         (UnkindedVar () (Ident () "b"))))
-- >   (UnkindedVar () (Ident () "c"))
data DeclHead l
    = DHead l (Name l) -- ^ type or class name
    | DHInfix l (TyVarBind l) (Name l) -- ^ infix application of the type/class name to the left operand
    | DHParen l (DeclHead l) -- ^ parenthesized declaration head
    | DHApp   l (DeclHead l) (TyVarBind l) -- ^ application to one more type variable
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A top-level declaration.
data Decl l
     = TypeDecl     l (DeclHead l) (Type l)
     -- ^ A type declaration
     | TypeFamDecl  l (DeclHead l) (Maybe (ResultSig l)) (Maybe (InjectivityInfo l))
     -- ^ A type family declaration
     | ClosedTypeFamDecl  l (DeclHead l) (Maybe (ResultSig l)) (Maybe (InjectivityInfo l)) [TypeEqn l]
     -- ^ A closed type family declaration
     | DataDecl     l (DataOrNew l) (Maybe (Context l)) (DeclHead l)                  [QualConDecl l] [Deriving l]
     -- ^ A data OR newtype declaration
     | GDataDecl    l (DataOrNew l) (Maybe (Context l)) (DeclHead l) (Maybe (Kind l)) [GadtDecl l]    [Deriving l]
     -- ^ A data OR newtype declaration, GADT style
     | DataFamDecl  l {-data-}      (Maybe (Context l)) (DeclHead l) (Maybe (ResultSig l))
     -- ^ A data family declaration
     | TypeInsDecl  l (Type l) (Type l)
     -- ^ A type family instance declaration
     | DataInsDecl  l (DataOrNew l) (Type l)                  [QualConDecl l] [Deriving l]
     -- ^ A data family instance declaration
     | GDataInsDecl l (DataOrNew l) (Type l) (Maybe (Kind l)) [GadtDecl l]    [Deriving l]
     -- ^ A data family instance declaration, GADT style
     | ClassDecl    l (Maybe (Context l)) (DeclHead l) [FunDep l] (Maybe [ClassDecl l])
     -- ^ A declaration of a type class
     | InstDecl     l (Maybe (Overlap l)) (InstRule l) (Maybe [InstDecl l])
     -- ^ An declaration of a type class instance
     | DerivDecl    l (Maybe (DerivStrategy l)) (Maybe (Overlap l)) (InstRule l)
     -- ^ A standalone deriving declaration
     | InfixDecl    l (Assoc l) (Maybe Int) [Op l]
     -- ^ A declaration of operator fixity
     | DefaultDecl  l [Type l]
     -- ^ A declaration of default types
     | SpliceDecl   l (Exp l)
     -- ^ A Template Haskell splicing declaration
     | TypeSig      l [Name l] (Type l)
     -- ^ A type signature declaration
     | PatSynSig    l [Name l] (Maybe [TyVarBind l]) (Maybe (Context l))
                               (Maybe [TyVarBind l]) (Maybe (Context l))
                                                     (Type l)
     -- ^ A pattern synonym signature declation
     | FunBind      l [Match l]
     -- ^ A set of function binding clauses
     | PatBind      l (Pat l) (Rhs l) {-where-} (Maybe (Binds l))
     -- ^ A pattern binding
     | PatSyn l (Pat l) (Pat l) (PatternSynDirection l)
     -- ^ A pattern synonym binding
     | ForImp       l (CallConv l) (Maybe (Safety l)) (Maybe String) (Name l) (Type l)
     -- ^ A foreign import declaration
     | ForExp       l (CallConv l)                    (Maybe String) (Name l) (Type l)
     -- ^ A foreign export declaration
     | RulePragmaDecl   l [Rule l]
     -- ^ A RULES pragma
     | DeprPragmaDecl   l [([Name l], String)]
     -- ^ A DEPRECATED pragma
     | WarnPragmaDecl   l [([Name l], String)]
     -- ^ A WARNING pragma
     | InlineSig        l Bool (Maybe (Activation l)) (QName l)
     -- ^ An INLINE pragma
     | InlineConlikeSig l      (Maybe (Activation l)) (QName l)
     -- ^ An INLINE CONLIKE pragma
     | SpecSig          l      (Maybe (Activation l)) (QName l) [Type l]
     -- ^ A SPECIALISE pragma
     | SpecInlineSig    l Bool (Maybe (Activation l)) (QName l) [Type l]
     -- ^ A SPECIALISE INLINE pragma
     | InstSig          l      (InstRule l)
     -- ^ A SPECIALISE instance pragma
     | AnnPragma        l (Annotation l)
     -- ^ An ANN pragma
     | MinimalPragma    l (Maybe (BooleanFormula l))
     -- ^ A MINIMAL pragma
     | RoleAnnotDecl    l (QName l) [Role l]
     -- ^ A role annotation
     | CompletePragma l [Name l] (Maybe (QName l))
     -- ^ A COMPLETE pragma
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

data ResultSig l = KindSig l (Kind l) | TyVarSig l (TyVarBind l)
  deriving (Eq, Ord, Show, Typeable, Data, Foldable, Traversable, Functor, Generic)

-- | Injectivity info for injective type families
data InjectivityInfo l = InjectivityInfo l (Name l) [Name l]
  deriving (Eq, Ord, Show, Typeable, Data, Foldable, Traversable, Functor, Generic)

-- | A type equation as found in closed type families.
data TypeEqn l = TypeEqn l (Type l) (Type l) deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A flag stating whether a declaration is a data or newtype declaration.
data DataOrNew l = DataType l | NewType l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A single constructor declaration within a data type declaration,
--   which may have an existential quantification binding.
data QualConDecl l
    = QualConDecl l
        {-forall-} (Maybe [TyVarBind l]) {- . -} (Maybe (Context l))
        {- => -} (ConDecl l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Declaration of an ordinary data constructor.
data ConDecl l
     = ConDecl l (Name l) [Type l]
                -- ^ ordinary data constructor
     | InfixConDecl l (Type l) (Name l) (Type l)
                -- ^ infix data constructor
     | RecDecl l (Name l) [FieldDecl l]
                -- ^ record constructor
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Declaration of a (list of) named field(s).
data FieldDecl l = FieldDecl l [Name l] (Type l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | The instance declaration rule, which is, roughly, the part of the instance declaration before the @where@ keyword.
--
-- Example: @instance Ord a => Ord (Maybe a)@ is represented as
--
-- >IRule
-- >   ()
-- >   Nothing
-- >   (Just
-- >      (CxSingle
-- >         ()
-- >         (ClassA
-- >            () (UnQual () (Ident () "Ord")) [ TyVar () (Ident () "a") ])))
-- >   (IHApp
-- >      ()
-- >      (IHCon () (UnQual () (Ident () "Ord")))
-- >      (TyParen
-- >         ()
-- >         (TyApp
-- >            ()
-- >            (TyCon () (UnQual () (Ident () "Maybe")))
-- >            (TyVar () (Ident () "a")))))
--
-- An optional explicit forall after @instance@ is supported:
-- @instance forall a . Ord a => Ord (Maybe a) where@ becomes
--
-- >IRule
-- >   ()
-- >   (Just [ UnkindedVar () (Ident () "a") ])
-- >   ...
data InstRule l
    = IRule l (Maybe [TyVarBind l]) (Maybe (Context l)) (InstHead l)
    | IParen l (InstRule l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- See bugs #7 and #31 for more details and use cases for the rationale
-- of the split. DeclOrInstHead should be used by DeclHead as the name implies.

-- | The instance head. The split between rule/head allow us to represent
-- @instance (Bounded a => Bounded [a]) where@ faithfully.
--
-- The structure of 'InstHead' follows one of 'DeclHead'.
--
-- For example, @instance C (Maybe a) Int where@ is represented as
--
-- >IHApp
-- >   ()
-- >   (IHApp
-- >      ()
-- >      (IHCon () (UnQual () (Ident () "C")))
-- >      (TyParen
-- >         ()
-- >         (TyApp
-- >            ()
-- >            (TyCon () (UnQual () (Ident () "Maybe")))
-- >            (TyVar () (Ident () "a")))))
-- >   (TyCon () (UnQual () (Ident () "Int")))))
data InstHead l
    = IHCon l (QName l) -- ^ type or class name
    | IHInfix l (Type l) (QName l) -- ^ infix application of the type/class name to the left operand
    | IHParen l (InstHead l) -- ^ parenthesized instance head
    | IHApp   l (InstHead l) (Type l) -- ^ application to one more type
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A single constructor declaration in a GADT data type declaration.
--
-- If the GADT is declared using the record syntax, e.g.
--
-- >data Ty where
-- >  TCon :: { field1 :: Int, field2 :: Bool } -> Ty
--
-- then the fields are stored as a list of 'FieldDecl's, and the final type
-- (@Ty@ in the above example) is stored in the last 'Type' field.
--
-- If the GADT is declared using the ordinary syntax, e.g.
--
-- >data Ty where
-- >  TCon :: Int -> Bool -> Ty
--
-- then @'Maybe' ['FieldDecl' l]@ is 'Nothing', and the whole constructor's
-- type (such as @Int -> Bool -> Ty@) is stored in the last 'Type' field.
data GadtDecl l
    = GadtDecl l (Name l)
        {-forall-} (Maybe [TyVarBind l]) {- . -} (Maybe (Context l))
        {- => -} (Maybe [FieldDecl l]) (Type l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Declarations inside a class declaration.
data ClassDecl l
    = ClsDecl    l (Decl l)
            -- ^ ordinary declaration
    | ClsDataFam l (Maybe (Context l)) (DeclHead l) (Maybe (ResultSig l))
            -- ^ declaration of an associated data type
    | ClsTyFam   l                     (DeclHead l) (Maybe (ResultSig l)) (Maybe (InjectivityInfo l))
            -- ^ declaration of an associated type synonym
    | ClsTyDef   l (TypeEqn l)
            -- ^ default choice for an associated type synonym
    | ClsDefSig  l (Name l) (Type l)
            -- ^ default signature
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A functional dependency, given on the form
--   l1 l2 ... ln -> r2 r3 .. rn
data FunDep l
    = FunDep l [Name l] [Name l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Recognised overlaps for overlap pragmas.
data Overlap l
    = NoOverlap l   -- ^ NO_OVERLAP pragma
    | Overlap l     -- ^ OVERLAP pragma
    | Overlapping l
    | Overlaps l
    | Overlappable l
    | Incoherent l  -- ^ INCOHERENT pragma
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Declarations inside an instance declaration.
data InstDecl l
    = InsDecl   l (Decl l)
            -- ^ ordinary declaration
    | InsType   l (Type l) (Type l)
            -- ^ an associated type definition
    | InsData   l (DataOrNew l) (Type l) [QualConDecl l] [Deriving l]
            -- ^ an associated data type implementation
    | InsGData  l (DataOrNew l) (Type l) (Maybe (Kind l)) [GadtDecl l] [Deriving l]
            -- ^ an associated data type implemented using GADT style
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Clauses of a function binding.
data Match l
     = Match l      (Name l) [Pat l]         (Rhs l) {-where-} (Maybe (Binds l))
        -- ^ A clause defined with prefix notation, i.e. the function name
        --  followed by its argument patterns, the right-hand side and an
        --  optional where clause.
     | InfixMatch l (Pat l) (Name l) [Pat l] (Rhs l) {-where-} (Maybe (Binds l))
        -- ^ A clause defined with infix notation, i.e. first its first argument
        --  pattern, then the function name, then its following argument(s),
        --  the right-hand side and an optional where clause.
        --  Note that there can be more than two arguments to a function declared
        --  infix, hence the list of pattern arguments.
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

data  PatternSynDirection l =
      Unidirectional -- ^ A unidirectional pattern synonym with "<-"
    | ImplicitBidirectional  -- ^ A bidirectional pattern synonym with "="
    | ExplicitBidirectional l [Decl l]  -- ^ A birectional pattern synonym with the construction specified.
    deriving (Eq, Ord, Show, Data, Typeable, Foldable, Traversable, Functor, Generic)

-- | The calling convention of a foreign function call.
data CallConv l
    = StdCall l
    | CCall l
    | CPlusPlus l
    | DotNet l
    | Jvm l
    | Js l
    | JavaScript l
    | CApi l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | The safety of a foreign function call.
data Safety l
    = PlayRisky l         -- ^ unsafe
    | PlaySafe l Bool     -- ^ safe ('False') or threadsafe ('True')
    | PlayInterruptible l -- ^ interruptible
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An annotation through an ANN pragma.
data Annotation l
    = Ann       l (Name l)  (Exp l)
    -- ^ An annotation for a declared name.
    | TypeAnn   l (Name l)  (Exp l)
    -- ^ An annotation for a declared type.
    | ModuleAnn l           (Exp l)
    -- ^ An annotation for the defining module.
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A boolean formula for MINIMAL pragmas.
data BooleanFormula l
    = VarFormula l (Name l)              -- ^ A variable.
    | AndFormula l [BooleanFormula l]    -- ^ And boolean formulas.
    | OrFormula l [BooleanFormula l]     -- ^ Or boolean formulas.
    | ParenFormula l (BooleanFormula l)  -- ^ Parenthesized boolean formulas.
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

data Role l
  = Nominal l
  | Representational l
  | Phantom l
  | RoleWildcard l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | The body of a RULES pragma.
data Rule l
    = Rule l String (Maybe (Activation l)) (Maybe [RuleVar l]) (Exp l) (Exp l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Variables used in a RULES pragma, optionally annotated with types
data RuleVar l
    = RuleVar l (Name l)
    | TypedRuleVar l (Name l) (Type l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Activation clause of a RULES pragma.
data Activation l
    = ActiveFrom   l Int
    | ActiveUntil  l Int
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

