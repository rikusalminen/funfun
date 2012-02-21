module FunFun.AST (
    Symbol,
    Expression(..),
    AST,
    freeVariables,
    freeVariables'
    ) where

import Text.Parsec (SourcePos)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import FunFun.Values

type Symbol = String

data Expression =
    Constant AtomValue |
    Identifier Symbol |
    Application |
    Lambda Symbol |
    Conditional |
    LetRec [(Symbol, AST)] |
    Let [(Symbol, AST)] |
    DoExpression
    deriving (Show, Eq)

type AST = Tree.Tree (Expression, SourcePos)

freeVariables' :: Set.Set Symbol -> AST -> Set.Set Symbol
freeVariables' bound (Tree.Node (Identifier sym, _) [])
    | not (sym `Set.member` bound) = Set.singleton sym
    | otherwise = Set.empty
freeVariables' bound (Tree.Node (Lambda sym, _) [body]) =
    freeVariables' (Set.insert sym bound) body
freeVariables' bound (Tree.Node (Let decls, pos) children) =
    freeVariables' bound $ Tree.Node (LetRec decls, pos) children
freeVariables' bound (Tree.Node (LetRec decls, _) [body]) =
    freeVariables' bound'' body
    where
    bound'' = Set.unions (bound':(map (freeVariables' bound' . snd) decls))
    bound' = Set.unions (bound:(map (Set.singleton . fst) decls))
freeVariables' bound (Tree.Node _ exprs) =
    Set.unions (map (freeVariables' bound) exprs)

freeVariables :: AST -> Set.Set Symbol
freeVariables =
    freeVariables' Set.empty
