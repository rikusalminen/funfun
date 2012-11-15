module FunFun.Types (
    TypeName,
    TypeExp(..),
    boolType,
    intType,
    floatType,
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
    | FunctionType [TypeExp] TypeExp
    | Constructor TypeName [TypeExp]
    deriving (Eq, Show)

boolType = Constructor "Bool" []
intType = Constructor "Int" []
floatType = Constructor "Float" []
stringType = Constructor "String" []

typeVarsInExp ::  TypeExp -> Set.Set TypeName
typeVarsInExp (TypeVar name) = Set.singleton name
typeVarsInExp (FunctionType args ret) = Set.unions . map typeVarsInExp $ ret:args
typeVarsInExp (Constructor _ args) = Set.unions . map typeVarsInExp $ args

type Substitution = Map.Map TypeName TypeExp

substitute ::  Substitution -> TypeExp -> TypeExp
substitute subst var@(TypeVar name) =
    case Map.lookup name subst of
      Just val -> substitute subst val
      Nothing -> var
substitute subst (FunctionType args ret) =
    FunctionType (map (substitute subst) args) (substitute subst ret)
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
