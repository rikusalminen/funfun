module FunFun.AST (
    Symbol,
    LetType(..),
    Expression(..),
    freeVariables,
    freeVariables'
    ) where

import Text.Parsec (SourcePos)
import qualified Data.Map as Map
import qualified Data.Set as Set

import FunFun.Values

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
        argument :: Expression,
        sourcePos :: SourcePos
        } |
    Lambda {
        lambdaArg :: Symbol,
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
        }
    deriving (Eq, Show)

freeVariables' :: Set.Set Symbol -> Expression -> Set.Set Symbol
freeVariables' bound (Variable sym _)
    | sym `Set.notMember` bound = Set.singleton sym
    | otherwise = Set.empty
freeVariables' bound (Lambda sym body _) =
    freeVariables' (Set.insert sym bound) body
freeVariables' bound (Let _ decls body _) =
    freeVariables' bound'' body
    where
    bound'' = Set.unions (bound':(map (freeVariables' bound' . snd) decls))
    bound' = Set.unions (bound:(map (Set.singleton . fst) decls))
freeVariables' bound (Application f x _) =
    freeVariables' bound f `Set.union` freeVariables' bound x
freeVariables' bound (Conditional cond cons alt _)=
    Set.unions (map (freeVariables' bound) [cond, cons, alt])
freeVariables' _ _ = Set.empty

freeVariables :: Expression -> Set.Set Symbol
freeVariables =
    freeVariables' Set.empty
