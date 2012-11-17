module Main where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import qualified Data.Map as Map
import Control.Exception (bracket)

import LLVM.FFI.Core hiding (sizeOf)
import LLVM.FFI.BitWriter

import Text.Parsec (parse)

import FunFun.Parser
import FunFun.DependencyAnalysis
import FunFun.Types
import FunFun.TypeChecker
import FunFun.LLVMCompiler
import FunFun.Pretty

compile :: String -> String -> IO ()
compile filename source = do
    let Right ast = parse parser filename source
    let optimized = depAnalysis ast
    -- no free variables!
    let typeEnv = Map.fromList [("x", Scheme [] (Constructor "Int" []))]
    let Right (sub, texp) = typeCheck typeEnv optimized
    let texp' = substitute sub texp

    bracket (withCString filename moduleCreateWithName) disposeModule $ \mod -> do
        let compilerEnv = []
        fun <- compileFunction mod compilerEnv "fooo" ["x"] optimized (FunctionType [texp'] texp')

        withCString (filename ++ ".bc") (writeBitcodeToFile mod)
        return ()

main =
    forever (getLine >>= (compile "repl"))
    where
    forever a = a >> forever a
