module FunFun.Interpreter where

import qualified Data.Map as Map
import qualified Data.Tree as Tree

import FunFun.AST
import FunFun.Values

data Value =
    Atom AtomValue |
    Closure Environment AST
    deriving (Eq, Show)

isTrue (Atom (IntValue x)) = x /= 0
isTrue (Atom (FloatValue x)) = x /= 0.0
isTrue (Atom (StringValue x)) = not . null $ x
isTrue (Closure _ _) = error "can't evaluate closure as boolean value"

type Environment = [Map.Map Symbol Value]
envLookup env sym =
    head [x | Just x <- map (Map.lookup sym) env]

eval :: Environment -> AST -> Value
eval env (Tree.Node (Constant val, _) []) =
    Atom val
eval env (Tree.Node (Identifier sym, _) []) =
    envLookup env sym
eval env (Tree.Node (Application, _) params) =
    foldr1 (apply env) (map (eval env) params)
eval env lambda@(Tree.Node (Lambda _, _) _) =
    Closure env lambda
eval env (Tree.Node (Conditional, _) [cond,cons,alt]) =
    eval env (if isTrue $ eval env cond then cons else alt)
eval env (Tree.Node (Let binds, _) [body]) =
    eval (Map.fromList [(name, eval env expr) | (name, expr) <- binds] : env) body
eval env (Tree.Node (DoExpression, _) exprs) =
    last (map (eval env) exprs)

apply :: Environment -> Value -> Value -> Value
apply _ (Closure env (Tree.Node (Lambda sym, _) [body])) value =
    eval (Map.singleton sym value : env) body
