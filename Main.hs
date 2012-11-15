module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.Parsec (parse)

import FunFun.AST
import FunFun.Parser
import FunFun.Pretty
import FunFun.Interpreter
import FunFun.DependencyAnalysis
import FunFun.Types
import FunFun.TypeChecker

testInterp src =
    case parse parser "" src of
        Left err -> print err
        Right ast -> do
            putStrLn . prettyprint $ ast
            let optimized = depAnalysis ast
            putStrLn . prettyprint $ optimized
            let typeEnv = Map.fromList [(x, Scheme [] (TypeVar x)) | x <- Set.toList (freeVariables optimized)]
            case typeCheck typeEnv optimized of
                Left err -> print err
                Right (sub, exp) -> do
                    let decls = [(TypeDecl scheme (Variable name undefined) undefined) | (name, scheme) <- Map.toList (substituteEnv sub typeEnv)]
                    mapM_ (putStrLn . prettyprint) decls
                    putStrLn (prettyprintType (substitute sub exp))


main = do
    forever (getLine >>= testInterp)
    where
    forever a = do { a ; forever a }
