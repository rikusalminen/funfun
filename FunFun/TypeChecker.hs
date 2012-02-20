module FunFun.TypeChecker where

import qualified Data.Tree as Tree
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.ST
import Data.STRef

import FunFun.Values
import FunFun.AST

type TypeName = String

data TypeExp =
    TypeVar TypeName
    | Constructor TypeName [TypeExp]
    deriving (Eq, Show)

arrow t1 t2 = Constructor "Arrow" [t1, t2]
intType = Constructor "Int" []

type Substitution = Map.Map TypeName TypeExp

typeVarsInExp ::  TypeExp -> Set.Set TypeName
typeVarsInExp (TypeVar name) = Set.singleton name
typeVarsInExp (Constructor _ args) = Set.unions . map typeVarsInExp $ args

substitute ::  Substitution -> TypeExp -> TypeExp
substitute subst var@(TypeVar name) =
    case Map.lookup name subst of
      Just val -> val
      Nothing -> var
substitute subst (Constructor name args) =
    Constructor name (map (substitute subst) args)

extend :: Substitution -> TypeName -> TypeExp -> Maybe Substitution
extend subst name val =
    extendVar subst name val
    where
    extendVar subst name val@(TypeVar var)
        | var == name  = Just subst
        | otherwise = extend' subst name val
    extendVar a b c = extend' a b c
    extend' subst name val
        | name `Set.member` typeVarsInExp val = Nothing -- occurs check
        | otherwise = Just (Map.insert name val subst)

unify :: Substitution -> TypeExp -> TypeExp -> Maybe Substitution
unify subst var@(TypeVar name) val
    | name `Map.notMember` subst = extend subst name (substitute subst val)
    | otherwise = unify subst (substitute subst var) (substitute subst val)
unify subst val var@(TypeVar name) =
    unify subst var val
unify subst (Constructor con ls) (Constructor con' rs)
    | con /= con' = Nothing -- type error
    | otherwise =
        foldr unify' (Just subst) (zip ls rs)
        where
        unify' _ Nothing = Nothing
        unify' (l, r) (Just subst) = unify subst l r

testCase1 = unify Map.empty (TypeVar "x") (TypeVar "y")
testCase2 = unify Map.empty (TypeVar "x") (Constructor "Foo" [TypeVar "y"])

type TypeEnv = Map.Map Symbol TypeExp

substituteEnv :: Substitution -> TypeEnv -> TypeEnv
substituteEnv subst env =
    Map.map (substitute subst) env

tc :: TypeEnv -> AST -> Maybe (Substitution, TypeExp)
tc env ast =
    runST $ do
    counter <- newSTRef (0 :: Integer)
    let newVar = do { x <- readSTRef counter ; writeSTRef counter (x+1) ; return $ show x }
    tc' newVar env ast

tc' newVar env ast = do
    tc'' env ast
    where
    tc'' env (Tree.Node (Constant (IntValue _), _) []) =
        return $ Just (Map.empty, intType)
    tc'' env (Tree.Node (Identifier sym, _) []) = do
        var <- newVar
        return $ Just (Map.empty, TypeVar (var ++ sym))
    tc'' env (Tree.Node (Application, _) [e1, e2]) = do
         ts <- tcList env [e1, e2]
         case ts of
           Just [t1, t2] -> do
               return $ Just (Map.empty, arrow t1 t2)
           _ -> Nothing

    tc'' _ _ =
        return Nothing
    tcList env [] =
        return $ Just (Map.empty, [])
    tcList env (e:es) = do
        first <- tc'' env e
        tcList' env es first
        where
        tcList' _ _ Nothing =
            return Nothing
        tcList' env es (Just (subst, t)) = do
            first <- tcList (substituteEnv subst env) es
            tcList'' subst t first
        tcList'' _ _ Nothing =
            return Nothing
        tcList'' subst t (Just (subst', ts)) =
            return $ Just (Map.union subst subst', (substitute subst' t) : ts)

tctest1 = tc Map.empty (Tree.Node (Constant (IntValue 1), undefined) [])
tctest2 = tc Map.empty (Tree.Node (Identifier "x", undefined) [])
