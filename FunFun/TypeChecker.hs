module FunFun.TypeChecker where

{-
    Based on Peter Hancock's type checker in Chapter 9 of the book
    "The Implementation of Functional Programming languages" by
    Simon L. Peyton Jones, et al. (1987)
    http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/
    -}

import Control.Monad.State
import Control.Monad.Error

import qualified Data.Tree as Tree
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
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
boolType = Constructor "Bool" []
intType = Constructor "Int" []
floatType = Constructor "Float" []
stringType = Constructor "String" []

type Substitution = Map.Map TypeName TypeExp

typeVarsInExp ::  TypeExp -> Set.Set TypeName
typeVarsInExp (TypeVar name) = Set.singleton name
typeVarsInExp (Constructor _ args) = Set.unions . map typeVarsInExp $ args

substitute ::  Substitution -> TypeExp -> TypeExp
substitute subst var@(TypeVar name) =
    case Map.lookup name subst of
      Just val -> substitute subst val
      Nothing -> var
substitute subst (Constructor name args) =
    Constructor name (map (substitute subst) args)

compose :: Substitution -> Substitution -> Substitution
compose left right =
    Map.union (Map.map (substitute left) right) left

type TypeError = String
type TC = ErrorT TypeError (State Integer)

runTC :: TC a -> Either String a
runTC x = fst $ runState (runErrorT x) 0

typeError :: String -> TC a
typeError = throwError

newTypeVar :: TC String
newTypeVar = lift $ do
    counter <- get
    put (counter+1)
    return (show counter)
newTypeVars 0 = return []
newTypeVars num = do { v <- newTypeVar ; tail <- newTypeVars (num-1) ; return (v:tail) }

extend :: Substitution -> TypeName -> TypeExp -> TC Substitution
extend subst name val =
    extendVar subst name val
    where
    extendVar subst name val@(TypeVar var)
        | var == name  = return subst
        | otherwise = extend' subst name val
    extendVar a b c = extend' a b c
    extend' subst name val
        | name `Set.member` typeVarsInExp val =
            typeError $
                "Occurs check - cannot construct infinite type substitution: " ++
                    show name ++ " = " ++ show val
        | otherwise =
            return $ Map.insert name val subst

unify :: Substitution -> (TypeExp, TypeExp) -> TC Substitution
unify subst (var@(TypeVar name), val)
    | name `Map.notMember` subst =
        extend subst name (substitute subst val)
    | otherwise =
        unify subst ((substitute subst var), (substitute subst val))
unify subst (val, var@(TypeVar name)) =
    unify subst (var, val)
unify subst ((Constructor con ls), (Constructor con' rs))
    | con /= con' =
        typeError $ "Constructors don't match: " ++ con ++ " /= " ++ con'
    | otherwise =
        foldM unify subst (zip ls rs)

testCase1 = runTC $ unify Map.empty ((TypeVar "x"),(TypeVar "y"))
testCase2 = runTC $ unify Map.empty ((TypeVar "x"),(Constructor "Foo" [TypeVar "y"]))

data TypeScheme = Scheme [TypeName] TypeExp

unknownsScheme :: TypeScheme -> Set.Set TypeName
unknownsScheme (Scheme vars exp) =
    typeVarsInExp exp `Set.intersection` (Set.fromList vars)

substituteScheme :: Substitution -> TypeScheme -> TypeScheme
substituteScheme subst (Scheme vars exp) =
    Scheme vars (substitute (subst `Map.difference` varsMap) exp)
    where
    varsMap = Map.fromList $ zip vars (repeat undefined)

type TypeEnv = Map.Map Symbol TypeScheme

unknownsEnv ::  Map.Map k TypeScheme -> Set.Set TypeName
unknownsEnv env =
    Set.unions (map unknownsScheme (Map.elems env))

substituteEnv :: Substitution -> TypeEnv -> TypeEnv
substituteEnv subst env =
    Map.map (substituteScheme subst) env

addDecls :: TypeEnv -> [TypeName] -> [TypeExp] -> TC TypeEnv
addDecls env names types = do
    schemes <- mapM (genbar (unknownsEnv env)) types
    return $ Map.union (Map.fromList (zip names schemes)) env
    where
    genbar unknowns t = do
        let vars = typeVarsInExp t `Set.difference` unknownsEnv env
        newVars <- newTypeVars (Set.size vars)
        let sub = Map.fromList [(old, TypeVar new) | (old, new) <- zip (Set.toList vars) newVars]
        return $ Scheme newVars (substitute sub t)

tcList :: TypeEnv -> [AST] -> TC (Substitution, [TypeExp])
tcList env xs =
    foldM tc1 (Map.empty, []) xs
    where
    tc1 (sub, exps) ast = do
        (sub', e) <- tc env ast
        return (compose sub' sub, exps ++ [e])

tc :: TypeEnv -> AST -> TC (Substitution, TypeExp)
tc env (Tree.Node (Constant value, _) []) =
    case value of
        (IntValue _) -> return (Map.empty, intType)
        (FloatValue _) -> return (Map.empty, floatType)
        (StringValue _) -> return (Map.empty, stringType)

tc env (Tree.Node (Identifier ident, _) []) = do
    newScheme <- newinstance scheme
    return (Map.empty, newScheme)
    where
    scheme = Maybe.fromJust $ Map.lookup ident env
    rename old new = Map.fromList [(sym, TypeVar var) | (sym, var) <- zip old new]
    newinstance (Scheme vars exp) = do
        newVars <- newTypeVars (length vars)
        return $ substitute (rename vars newVars) exp

tc env (Tree.Node (Application, _) [e1, e2]) = do
    (sub, [t1, t2]) <- tcList env [e1, e2]
    varName <- newTypeVar
    let var = TypeVar varName
    sub' <- unify sub (t1, t2 `arrow` var)
    return (sub', substitute sub' var)

tc env (Tree.Node (Lambda sym, _) [body]) = do
    varName <- newTypeVar
    let var = TypeVar varName
    let env' = Map.insert sym (Scheme [] var) env
    (sub, t) <- tc env' body
    return (sub, (substitute sub var) `arrow` t)

tc env (Tree.Node (Conditional, _) [cond, cons, alt]) = do
    (sub, [condt, const, altt]) <- tcList env [cond, cons, alt]
    sub' <- unify sub (condt, boolType)
    sub'' <- unify sub' (const, altt)
    return (sub'', substitute sub'' const)

tc env (Tree.Node (Let decls, _) [body]) = do
    let (names, vals) = unzip decls
    (sub, ts) <- tcList env vals
    env' <- addDecls (substituteEnv sub env) names ts
    tc env' body

tc env (Tree.Node (LetRec decls, _) [body]) = do
    newVars <- newTypeVars (length decls)
    let (names, vals) = unzip decls
    let newEnv = (Map.fromList [(name, Scheme [] (TypeVar var)) | (name, var) <- zip names newVars])
    (sub, ts) <- tcList (newEnv `Map.union` env) vals

    let env' = substituteEnv sub env
    let newEnv' = substituteEnv sub newEnv
    let ts' = [x | (Scheme [] x) <- map (Maybe.fromJust . flip Map.lookup newEnv') names]
    sub' <- foldM unify sub (zip ts ts')

    let newEnv'' = substituteEnv sub' newEnv'
    let ts'' = [x | (Scheme [] x) <- map (Maybe.fromJust . flip Map.lookup newEnv'') names]
    env'' <- addDecls (substituteEnv sub' env') names ts''

    (sub'', t) <- tc env'' body
    return $ (compose sub' sub'', t)

tc _ _ = typeError "not defined"

typeCheck :: TypeEnv -> AST -> Either TypeError (Substitution, TypeExp)
typeCheck env ast = runTC (tc env ast)


typeTest :: AST -> Either TypeError (Substitution, TypeExp)
typeTest ast =
    typeCheck env ast
    where
    env = Map.fromList
        [(sym, Scheme [] (TypeVar $ sym)) |
            sym <- Set.toList (freeVariables ast)]
tcTest1 =
    typeCheck Map.empty (Tree.Node (Constant (IntValue undefined), undefined) [])
tcTest2 =
    typeCheck
        (Map.fromList [("f", Scheme ["alpha"] (TypeVar "alpha"))])
        (Tree.Node (Identifier "f", undefined) [])
tcTest3 =
    typeCheck
        (Map.fromList [("f", Scheme [] (TypeVar "alpha"))])
        (Tree.Node (Application, undefined) [
            (Tree.Node (Identifier "f", undefined) []),
            (Tree.Node (Constant (IntValue undefined), undefined) [])
            ])
env3 = (Map.fromList [("f", Scheme ["alpha"] (TypeVar "alpha"))])
source3 = [
        (Tree.Node (Identifier "f", undefined) []),
        (Tree.Node (Constant (IntValue undefined), undefined) [])
        ]
tcTest4 =
    typeCheck
        (Map.fromList [("f", Scheme ["alpha", "beta"] (TypeVar "alpha" `arrow` (TypeVar "alpha" `arrow` TypeVar "beta")))])
        (Tree.Node (Application, undefined) [
            (Tree.Node (Identifier "f", undefined) []),
            (Tree.Node (Constant (IntValue undefined), undefined) [])
            ])
tcTest5 =
    typeCheck
        Map.empty
        (Tree.Node (Lambda "x", undefined) [Tree.Node (Constant (IntValue 1), undefined) []])
tcTest6 =
    typeCheck
        Map.empty
        (Tree.Node (Lambda "x", undefined) [Tree.Node (Identifier "x", undefined) []])
