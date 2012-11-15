module FunFun.AST (
    Symbol,
    LetType(..),
    Expression(..),
    freeVariables,
    ) where

import Text.Parsec (SourcePos)
import qualified Data.Map as Map
import qualified Data.Set as Set

import FunFun.Values
import FunFun.Types

type Symbol = String

data LetType = Rec | NonRec
    deriving (Eq, Ord, Show)

data Expression =
    Constant {
        constValue :: AtomValue,
        sourcePos :: SourcePos
        } |
    Variable {
        symbol :: Symbol,
        sourcePos :: SourcePos
        } |
    Application {
        function :: Expression,
        arguments :: [Expression],
        sourcePos :: SourcePos
        } |
    Lambda {
        lambdaArgs :: [Symbol],
        lambdaBody :: Expression,
        sourcePos :: SourcePos
        } |
    Conditional {
        condition :: Expression,
        consequent :: Expression,
        alternative :: Expression,
        sourcePos :: SourcePos
        } |
    Let {
        letType :: LetType,
        letDeclarations :: [(Symbol, Expression)],
        letBody :: Expression,
        sourcePos :: SourcePos
        } |
    TypeDecl {
        typeDecl :: TypeScheme,
        typeDeclBody :: Expression,
        sourcePos :: SourcePos
        }
    deriving (Eq, Show)

freeVariables :: Expression -> Set.Set Symbol
freeVariables (Constant _ _) =
    Set.empty
freeVariables (Variable sym _) =
    Set.singleton sym
freeVariables (Lambda args body _) =
    freeVariables body `Set.difference` Set.fromList args
freeVariables (Let _ decls body _) =
    Set.unions (map freeVariables (body:map snd decls)) `Set.difference` Set.fromList (map fst decls)
freeVariables (Application fun args _) =
    Set.unions . map freeVariables $ fun:args
freeVariables (Conditional cond cons alt _) =
    Set.unions (map freeVariables [cond, cons, alt])
freeVariables (TypeDecl _ body _) =
    freeVariables body
