{-# LANGUAGE ScopedTypeVariables #-}
module FunFun.LLVMCompiler (
    CompiledValue(..),
    compileType,
    compileExp,
    compileFunction,
    compileModule,
    builtinPlus,
    builtinMinus,
    builtinMul,
    builtinEq
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


emitCall :: BuilderRef -> String -> ValueRef -> [ValueRef] -> IO ValueRef
emitCall builder name fun args = do
    withCString name $ \cname -> do
        allocaBytes (length args * sizeOf (undefined :: ValueRef)) $ \(ptr :: Ptr ValueRef) -> do
            mapM_ (uncurry (pokeElemOff ptr)) [(i, ref) | (i, ref) <- zip [0..] args]
            buildCall builder fun ptr (fromIntegral . length $ args) cname


apply :: ValueRef -> BuilderRef -> CompilerEnv -> CompiledValue -> [CompiledValue] -> TypeExp -> IO CompiledValue
apply f builder env (CompiledValue ref texp) args typ = do
    ref <- emitCall builder "" ref [val | (CompiledValue val _) <- args]
    return $ CompiledValue ref typ
apply f builder env (CompiledBuiltIn emit) args typ =
    emit f builder env args typ

compileExp :: ValueRef -> BuilderRef -> CompilerEnv -> TypeEnv -> Expression -> TypeExp -> IO CompiledValue
compileExp _ _ _ _ (Constant (IntValue i) _) typ = do
    return $ CompiledValue (constInt (compileType typ) (fromIntegral i) (fromIntegral 0)) typ

compileExp _ _ _ _ (Constant (FloatValue f) _) typ =
    return $ CompiledValue (constReal (compileType typ) (CDouble f)) typ

compileExp _ _ env tenv exp@(Variable ident _) typ = do
    return . fromJust . lookupEnv env $ ident

compileExp f builder env tenv (Application fun args _) typ = do
    let Right funt@(FunctionType argt rett) = runTC $ do
        (sub, (funt:argt)) <- tcList tenv (fun:args)
        sub' <- unify sub (funt, FunctionType argt typ)
        return $ substitute sub' funt

    fun' <- compileExp f builder env tenv fun funt
    args' <- mapM (\(e, t) -> compileExp f builder env tenv e t) $ zip args argt
    apply f builder env fun' args' rett

compileExp f builder env tenv (Conditional cond cons alt _) typ = do
    (CompiledValue condv _) <- compileExp f builder env tenv cond (Constructor "Bool" [])

    thenbb <- withCString "then" (appendBasicBlock f)
    elsebb <- withCString "else" (appendBasicBlock f)
    endbb <- withCString "endif" (appendBasicBlock f)

    buildCondBr builder condv thenbb elsebb

    positionAtEnd builder thenbb
    (CompiledValue thenv _) <- compileExp f builder env tenv cons typ
    buildBr builder endbb

    positionAtEnd builder elsebb
    (CompiledValue elsev _) <- compileExp f builder env tenv alt typ
    buildBr builder endbb

    positionAtEnd builder endbb

    phi <- phinode "phi" [(thenv, thenbb), (elsev, elsebb)]

    return $ CompiledValue phi typ
    where
    phinode :: String -> [(ValueRef, BasicBlockRef)] -> IO ValueRef
    phinode name incoming = do
        phi <- withCString name (buildPhi builder (compileType typ))
        allocaBytes (length incoming * sizeOf (undefined :: ValueRef)) $ \(valptr :: Ptr ValueRef) -> do
            allocaBytes (length incoming * sizeOf (undefined :: ValueRef)) $ \(bbptr :: Ptr ValueRef) -> do
                mapM_ (uncurry (pokeElemOff valptr)) [(i, val) | (i, (val, _)) <- zip [0..] incoming]
                mapM_ (uncurry (pokeElemOff bbptr)) [(i, basicBlockAsValue bb) | (i, (_, bb)) <- zip [0..] incoming]
                addIncoming phi valptr bbptr (fromIntegral $ length incoming)
                return phi

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

    let frame = Map.fromList [(sym, CompiledValue (getParam f (fromIntegral i)) typ) | (i, sym, typ) <- zip3 [0..] args argt]
    let frame' = Map.fromList [(name, CompiledValue f typ)]
    let env' = frame : frame' : env

    let tframe = Map.fromList [(sym, Scheme [] texp) | (sym, texp) <- zip args argt]
    let tframe' = Map.fromList [(name, Scheme [] typ)]
    let tenv' = tframe `Map.union` tframe' `Map.union` tenv

    bb <- withCString "" (appendBasicBlock f)

    builder <- createBuilder
    positionAtEnd builder bb

    (CompiledValue v _) <- compileExp f builder env' tenv' exp ret

    ret <- buildRet builder v
    return f

compileModule :: CompilerEnv -> String -> [(Symbol, (Expression, TypeScheme))] -> IO ModuleRef
compileModule =
    undefined

builtinIOp buildfun _ builder _ [l@(CompiledValue v1 t1), r@(CompiledValue v2 t2)] texp = do
    let name = "add"
    val <- withCString name (buildfun builder v1 v2)
    return $ CompiledValue val texp

builtinPlus = builtinIOp buildAdd
builtinMinus = builtinIOp buildSub
builtinMul = builtinIOp buildMul

{-
-- From LLVM Core.h
00317 typedef enum {
00318   LLVMIntEQ = 32, /**< equal */
00319   LLVMIntNE,      /**< not equal */
00320   LLVMIntUGT,     /**< unsigned greater than */
00321   LLVMIntUGE,     /**< unsigned greater or equal */
00322   LLVMIntULT,     /**< unsigned less than */
00323   LLVMIntULE,     /**< unsigned less or equal */
00324   LLVMIntSGT,     /**< signed greater than */
00325   LLVMIntSGE,     /**< signed greater or equal */
00326   LLVMIntSLT,     /**< signed less than */
00327   LLVMIntSLE      /**< signed less or equal */
00328 } LLVMIntPredicate;
-}
builtinEq :: (ValueRef -> BuilderRef -> CompilerEnv -> [CompiledValue] -> TypeExp -> IO CompiledValue)
builtinEq _ builder _ [l@(CompiledValue v1 t1), r@(CompiledValue v2 t2)] texp = do
    let name = "eq"
    val <- withCString name (buildICmp builder (fromIntegral 32) v1 v2)
    return $ CompiledValue val texp
