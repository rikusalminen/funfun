{-# LANGUAGE ScopedTypeVariables #-}
module FunFun.LLVMCompiler (
    CompiledValue(..),
    compileType,
    compileExp,
    compileFunction,
    compileModule,
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
import FunFun.TypeChecker
import FunFun.Pretty

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


apply :: ValueRef -> BuilderRef -> CompilerEnv -> CompiledValue -> [CompiledValue] -> TypeExp -> IO CompiledValue
apply f builder env (CompiledValue ref texp) args typ =
    undefined -- TODO: emit "invoke" instruction
apply f builder env (CompiledBuiltIn emit) args typ =
    emit f builder env args typ

compileExp :: ValueRef -> BuilderRef -> CompilerEnv -> TypeEnv -> Expression -> TypeExp -> IO CompiledValue
compileExp _ _ _ _ (Constant (IntValue i) _) typ = do
    return $ CompiledValue (constInt (compileType typ) (fromIntegral i) (fromIntegral 0)) typ
compileExp _ _ _ _ (Constant (FloatValue f) _) typ =
    return $ CompiledValue (constReal (compileType typ) (CDouble f)) typ
compileExp _ _ env tenv (Variable ident _) typ =
    -- TODO: check type
    return . fromJust . lookupEnv env $ ident
compileExp f builder env tenv (Application fun args _) typ = do
    let Right funt@(FunctionType argt rett) = runTC $ do
        (sub, (funt:argt)) <- tcList tenv (fun:args)
        sub' <- unify sub (funt, FunctionType argt typ)
        return $ substitute sub' funt

    fun' <- compileExp f builder env tenv fun funt
    args' <- mapM (\(e, t) -> compileExp f builder env tenv e t) $ zip args argt
    apply f builder env fun' args' rett
compileExp _ _ _ _ _ _ =
    error "Invalid expression"

createFunction :: ModuleRef -> String -> TypeExp -> Linkage -> IO ValueRef
createFunction mod name typ@(FunctionType _ _) linkage = do
    f <- withCString name (\cname -> addFunction mod cname (compileType typ))
    setLinkage f (fromLinkage linkage)
    return f

compileFunction :: ModuleRef -> CompilerEnv -> TypeEnv -> String -> [Symbol] -> Expression -> TypeExp -> IO ValueRef
compileFunction mod env tenv name args exp typ@(FunctionType argt ret) = do
    f <- createFunction mod name typ ExternalLinkage

    bb <- withCString "" (appendBasicBlock f)
    builder <- createBuilder
    positionAtEnd builder bb

    let frame = Map.fromList [(sym, CompiledValue (getParam f (fromIntegral i)) typ) | (i, sym, typ) <- zip3 [0..] args argt]
    let frame' = Map.fromList [(name, CompiledValue f typ)]
    let env' = frame : frame' : env

    let tframe = Map.fromList [(sym, Scheme [] texp) | (sym, texp) <- zip args argt]
    let tframe' = Map.fromList [(name, Scheme [] typ)]
    let tenv' = tframe `Map.union` tframe' `Map.union` tenv

    e <- compileExp f builder env' tenv' exp ret
    let val e = case e of -- TODO: clean this mess up
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
