module FunFun.Types (
    TypeName,
    TypeExp(..),
    arrow,
    boolType,
    intType,
    floatType,
    funArgs,
    funRet,
    stringType,
    typeVarsInExp,
    Substitution(..),
    substitute,
    compose,
    TypeScheme(..),
    unknownsScheme,
    substituteScheme,
    TypeEnv(..),
    unknownsEnv,
    substituteEnv
    ) where

import qualified Data.Set as Set
import qualified Data.Map as Map

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

funArgs (Constructor "Arrow" [l, r]) =
    l : funArgs r
funArgs _ = []

funRet (Constructor "Arrow" [_, r]) =
    funRet r
funRet x = x

typeVarsInExp ::  TypeExp -> Set.Set TypeName
typeVarsInExp (TypeVar name) = Set.singleton name
typeVarsInExp (Constructor _ args) = Set.unions . map typeVarsInExp $ args

type Substitution = Map.Map TypeName TypeExp

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

data TypeScheme =
    Scheme [TypeName] TypeExp
    deriving (Eq, Show)

unknownsScheme :: TypeScheme -> Set.Set TypeName
unknownsScheme (Scheme vars exp) =
    typeVarsInExp exp `Set.intersection` (Set.fromList vars)

substituteScheme :: Substitution -> TypeScheme -> TypeScheme
substituteScheme subst (Scheme vars exp) =
    Scheme vars (substitute (subst `Map.difference` varsMap) exp)
    where
    varsMap = Map.fromList $ zip vars (repeat undefined)

type TypeEnv = Map.Map String TypeScheme

unknownsEnv ::  Map.Map k TypeScheme -> Set.Set TypeName
unknownsEnv env =
    Set.unions (map unknownsScheme (Map.elems env))

substituteEnv :: Substitution -> TypeEnv -> TypeEnv
substituteEnv subst env =
    Map.map (substituteScheme subst) env
