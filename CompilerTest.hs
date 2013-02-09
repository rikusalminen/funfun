module Main where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Exception (bracket)

import LLVM.FFI.Core hiding (sizeOf)
import LLVM.FFI.BitWriter

import Text.Parsec (parse)

import FunFun.Parser
import FunFun.AST
import FunFun.DependencyAnalysis
import FunFun.Types
import FunFun.TypeChecker
import FunFun.LLVMCompiler
import FunFun.Pretty

compile :: String -> String -> IO ()
compile filename source = do
    let Right ast = parse parser filename source

    let freeVars = freeVariables ast `Set.difference` builtInSymbols
    let ast' = if freeVars == Set.empty then ast else Lambda (Set.toList freeVars) ast (sourcePos ast)

    let optimized = depAnalysis ast'
    let Right (sub, texp) = typeCheck builtInTypeEnv optimized
    let texp' = substitute sub texp

    let typ = case texp' of
            typ@(FunctionType _ _) -> typ
            t -> FunctionType [] t

    putStrLn $ (prettyprint optimized) ++ " :: " ++ (prettyprintType typ)

    bracket (withCString filename moduleCreateWithName) disposeModule $ \mod -> do
        fun <- case optimized of
            (Lambda args body _) ->
                compileFunction mod builtInCompilerEnv builtInTypeEnv "lambda" args body typ []
            _ ->
                compileFunction mod builtInCompilerEnv builtInTypeEnv "fooo" [] optimized typ []

        withCString (filename ++ ".bc") (writeBitcodeToFile mod)
        return ()

main =
    forever (getLine >>= (compile "repl"))
    where
    forever a = a >> forever a
