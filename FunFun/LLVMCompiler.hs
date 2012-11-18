{-# LANGUAGE ScopedTypeVariables #-}
module FunFun.LLVMCompiler (
    CompiledValue(..),
    compileType,
    compileExp,
    compileFunction,
    compileModule,
    compileTest,
    builtinPlus
    ) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Exception (bracket)

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)

import LLVM.FFI.Core hiding (sizeOf)
import LLVM.FFI.BitWriter

import FunFun.AST
import FunFun.Types hiding (floatType)
import FunFun.Values

funType :: TypeRef -> [TypeRef] -> TypeRef
funType ret args =
    unsafePerformIO $ do
    let varargs = 0 -- nonzero if varargs
    allocaBytes (length args * sizeOf (undefined :: TypeRef)) $ \(ptr :: Ptr TypeRef) -> do
        mapM_ (uncurry (pokeElemOff ptr)) [(i, ref) | (i, ref) <- zip [0..] args]
        return $ functionType ret ptr (fromIntegral . length $ args) varargs

compileType :: TypeExp -> TypeRef
compileType (TypeVar _) =
    error "Can't compile types with type variables"
compileType (Constructor "Int" []) =
    int32Type
compileType (Constructor "Float" []) =
    floatType
compileType (Constructor "Bool" []) =
    int1Type
compileType (FunctionType args ret) =
    funType (compileType ret) (map compileType args)
compileType _ =
    error "Unknown type"

data CompiledValue =
    CompiledValue ValueRef TypeExp |
    CompiledBuiltIn (ValueRef -> BuilderRef -> CompilerEnv -> [CompiledValue] -> TypeExp -> IO CompiledValue)

instance Show CompiledValue where
    show (CompiledValue ref texp) = "CompiledValue " ++ show ref ++ " (" ++ show texp ++ ")"
    show (CompiledBuiltIn _) = "CompiledBuiltIn"

type CompilerEnv = [Map.Map Symbol CompiledValue]

lookupEnv :: CompilerEnv -> Symbol -> Maybe CompiledValue
lookupEnv [] _ = Nothing
lookupEnv (frame:frames) ident =
    case Map.lookup ident frame of
      Just x -> Just x
      Nothing -> lookupEnv frames ident

compileExp :: ValueRef -> BuilderRef -> CompilerEnv -> Expression -> TypeExp -> IO CompiledValue
compileExp _ _ _ (Constant (IntValue i) _) typ = do
    --return $ CompiledValue (constInt (compileType typ) (fromIntegral i) (fromIntegral 0)) typ
    let typ' = (Constructor "Int" [])
    return $ CompiledValue (constInt (compileType typ') (fromIntegral i) (fromIntegral 0)) typ'
compileExp _ _ _ (Constant (FloatValue f) _) typ =
    return $ CompiledValue (constReal (compileType typ) (CDouble f)) typ
compileExp _ _ env (Variable ident _) typ =
    return . fromJust . lookupEnv env $ ident
compileExp f builder env (Application fun args _) typ = do
    fun' <- compileExp f builder env fun undefined -- TODO: fix undefined
    args' <- mapM (\a -> compileExp f builder env a undefined) args

    case fun' of
      (CompiledValue ref texp) -> do
          undefined
      (CompiledBuiltIn emit) ->
          emit f builder env args' typ
compileExp _ _ _ _ _ =
    error "Invalid expression"

compileFunction :: ModuleRef -> CompilerEnv -> String -> [Symbol] -> Expression -> TypeExp -> IO ValueRef
compileFunction mod env name args exp typ@(FunctionType argt ret) = do
    let ft = compileType typ
    f <- withCString name (\cs -> addFunction mod cs ft)
    setLinkage f (fromLinkage ExternalLinkage)

    bb <- withCString "" (appendBasicBlock f)
    builder <- createBuilder
    positionAtEnd builder bb

    let frame = Map.fromList [(sym, CompiledValue (getParam f (fromIntegral i)) typ) | (i, sym, typ) <- zip3 [0..] args argt]
    let frame' = Map.fromList [(name, CompiledValue f typ)]
    let env' = frame : frame' : env

    e <- compileExp f builder env' exp ret
    let val e = case e of
            CompiledValue val typ -> return val
            CompiledBuiltIn emit -> val =<< emit f builder env [] ret
    v <- val e

    ret <- buildRet builder v
    return f

compileModule :: CompilerEnv -> String -> [(Symbol, (Expression, TypeScheme))] -> IO ModuleRef
compileModule =
    undefined

builtinPlus :: (ValueRef -> BuilderRef -> CompilerEnv -> [CompiledValue] -> TypeExp -> IO CompiledValue)
builtinPlus _ builder _ [l@(CompiledValue v1 t1), r@(CompiledValue v2 t2)] texp = do
    let name = "add"
    val <- withCString name (buildAdd builder v1 v2)
    return $ CompiledValue val texp

compileTest name = do
    let modname = name
    let filename = name ++ ".bc"
    --let exp = Variable "x" undefined
    let exp = Constant (IntValue 1234) undefined
    let args = ["x"]
    let typ = FunctionType [Constructor "Int" []] (Constructor "Int" [])
    -- let args = []
    -- let typ = Constructor "Int" []
    bracket (withCString modname moduleCreateWithName) disposeModule $ \mod -> do
        let env = []
        compileFunction mod env name args exp typ
        withCString filename (writeBitcodeToFile mod)


