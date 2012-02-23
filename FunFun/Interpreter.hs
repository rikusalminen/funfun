module FunFun.Interpreter where

import qualified Data.Map as Map

import FunFun.AST
import FunFun.Values

data Value =
    Atom AtomValue |
    Closure Environment Expression
    deriving (Eq, Show)

isTrue (Atom (IntValue x)) = x /= 0
isTrue (Atom (FloatValue x)) = x /= 0.0
isTrue (Atom (StringValue x)) = not . null $ x
isTrue (Closure _ _) = error "can't evaluate closure as boolean value"

type Environment = [Map.Map Symbol Value]
envLookup env sym =
    head [x | Just x <- map (Map.lookup sym) env]

eval :: Environment -> Expression -> Value
eval env (Constant val _) =
    Atom val
eval env (Variable sym _) =
    envLookup env sym
eval env (Application fun arg _) =
    apply (eval env fun) (eval env arg)
eval env lambda@(Lambda _ _ _) =
    Closure env lambda
eval env (Conditional cond cons alt _) =
    eval env (if isTrue $ eval env cond then cons else alt)
eval env (Let NonRec binds body _) =
    eval (Map.fromList [(name, eval env expr) | (name, expr) <- binds] : env) body

apply :: Value -> Value -> Value
apply (Closure env (Lambda sym body _)) value =
    eval (Map.singleton sym value : env) body
