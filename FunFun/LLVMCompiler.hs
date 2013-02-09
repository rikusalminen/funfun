{-# LANGUAGE ScopedTypeVariables #-}
module FunFun.LLVMCompiler (
    CompiledValue(..),
    compileType,
    compileExp,
    compileFunction,
    compileModule,
    builtInTypeEnv,
    builtInCompilerEnv,
    builtInSymbols
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Data.Maybe (fromJust)
import Control.Exception (bracket)
import Control.Monad (when)

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
import FunFun.DependencyAnalysis
import FunFun.TypeChecker
import FunFun.Pretty

struct :: [TypeRef] -> TypeRef
struct elements =
    unsafePerformIO $ do
    let packed = 0
    allocaBytes (length elements * sizeOf (undefined :: TypeRef)) $ \(ptr :: Ptr TypeRef) -> do
        mapM_ (uncurry (pokeElemOff ptr)) [(i, ref) | (i, ref) <- zip [0..] elements]
        return $ structType ptr (fromIntegral . length $ elements) packed

funType :: TypeRef -> [TypeRef] -> TypeRef
funType ret args =
    unsafePerformIO $ do
    let varargs = 0 -- nonzero if varargs
    allocaBytes (length args * sizeOf (undefined :: TypeRef)) $ \(ptr :: Ptr TypeRef) -> do
        mapM_ (uncurry (pokeElemOff ptr)) [(i, ref) | (i, ref) <- zip [0..] args]
        return $ functionType ret ptr (fromIntegral . length $ args) varargs

getelementptr :: BuilderRef -> ValueRef -> [ValueRef] -> String -> IO ValueRef
getelementptr builder value indices name =
    allocaBytes (length indices * sizeOf (undefined :: ValueRef)) $ \(ptr :: Ptr ValueRef) -> do
        mapM_ (uncurry (pokeElemOff ptr)) [(i, ref) | (i, ref) <- zip [0..] indices]
        withCString name (buildGEP builder value ptr (fromIntegral . length $ indices))

getelementptrInt :: BuilderRef -> ValueRef -> [Int] -> String -> IO ValueRef
getelementptrInt builder value indices name =
   getelementptr builder value [constInt int32Type (fromIntegral i) (fromIntegral 0) | i <- indices] name

getelementptrConst :: ValueRef -> [ValueRef] -> ValueRef
getelementptrConst value indices =
    unsafePerformIO $
    allocaBytes (length indices * sizeOf (undefined :: ValueRef)) $ \(ptr :: Ptr ValueRef) -> do
        mapM_ (uncurry (pokeElemOff ptr)) [(i, ref) | (i, ref) <- zip [0..] indices]
        return $ constGEP value ptr (fromIntegral . length $ indices)

getelementptrConstInt :: ValueRef -> [Int] -> ValueRef
getelementptrConstInt value indices =
   getelementptrConst value [constInt int32Type (fromIntegral i) (fromIntegral 0) | i <- indices]

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

type CompilerCtx = (ModuleRef, ValueRef, BasicBlockRef, BuilderRef, (CompilerEnv, TypeEnv))

data CompiledValue =
    CompiledValue ValueRef TypeExp |
    CompiledBuiltIn (CompilerCtx -> CompilerEnv -> TypeEnv -> [CompiledValue] -> TypeExp -> IO CompiledValue)

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

apply :: CompilerCtx -> CompilerEnv -> TypeEnv -> CompiledValue -> [CompiledValue] -> TypeExp -> IO CompiledValue
apply (_, _, _, builder, _) env tenv (CompiledValue ref texp) args typ = do
    ref <- emitCall builder "" ref [val | (CompiledValue val _) <- args]
    return $ CompiledValue ref typ
apply ctx env tenv (CompiledBuiltIn emit) args typ =
    emit ctx env tenv args typ

compileExp :: CompilerCtx -> CompilerEnv -> TypeEnv -> Expression -> TypeExp -> IO CompiledValue
compileExp _ _ _ (Constant (IntValue i) _) typ = do
    return $ CompiledValue (constInt (compileType typ) (fromIntegral i) (fromIntegral 0)) typ

compileExp _ _ _ (Constant (FloatValue f) _) typ =
    return $ CompiledValue (constReal (compileType typ) (CDouble f)) typ

compileExp _ env tenv exp@(Variable ident _) typ = do
    return . fromJust . lookupEnv env $ ident

compileExp ctx env tenv (Application fun args _) typ = do
    let Right funt@(FunctionType argt rett) = runTC $ do
        (sub, (funt:argt)) <- tcList tenv (fun:args)
        sub' <- unify sub (funt, FunctionType argt typ)
        return $ substitute sub' funt

    fun' <- compileExp ctx env tenv fun funt
    args' <- mapM (\(e, t) -> compileExp ctx env tenv e t) $ zip args argt
    apply ctx env tenv fun' args' rett

compileExp ctx@(mod, f, bb, builder, modenvs) env tenv (Conditional cond cons alt _) typ = do
    (CompiledValue condv _) <- compileExp ctx env tenv cond (Constructor "Bool" [])

    endbb <- withCString "endif" (insertBasicBlock bb)
    elsebb <- withCString "else" (insertBasicBlock endbb)
    thenbb <- withCString "then" (insertBasicBlock elsebb)

    buildCondBr builder condv thenbb elsebb

    positionAtEnd builder thenbb
    (CompiledValue thenv _) <- compileExp (mod, f, elsebb, builder, modenvs) env tenv cons typ
    buildBr builder endbb
    thenbb' <- getInsertBlock builder

    positionAtEnd builder elsebb
    (CompiledValue elsev _) <- compileExp (mod, f, endbb, builder, modenvs) env tenv alt typ
    buildBr builder endbb
    elsebb' <- getInsertBlock builder

    positionAtEnd builder endbb
    phi <- phinode "phi" [(thenv, thenbb'), (elsev, elsebb')]

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

compileExp ctx env tenv exp@(TypeDecl scheme exp' _) typ = do
    let (Right typ') = runTC $ do
        (sub, t) <- tc tenv exp
        sub' <- unify sub (t, typ)
        return $ substitute sub' typ

    compileExp ctx env tenv exp' typ'

compileExp (mod, _, _, builder, (modenv, modtenv)) env tenv exp@(Lambda args body _) typ
    | typeVarsInExp typ /= Set.empty =
        -- TODO: implement some kind of "templates"
        error "Cannot compile polymorphic lambda functions"
    | freeVariables exp `Set.difference` Set.unions (map Map.keysSet modenv) == Set.empty = do
        f <- compileFunction mod modenv modtenv "lambda" args body typ []
        return $ CompiledValue f typ
    | otherwise = do
        let freevar = Set.toList $ freeVariables exp `Set.difference` Set.unions (map Map.keysSet modenv)
        let lookupType name = let Just (Scheme [] texp) = Map.lookup name tenv in texp
        let captured = [(name, lookupType name) | name <- freevar]
        let closureType = struct (map (compileType . snd) captured)

        fun <- compileFunction mod modenv modtenv "lambda" args body typ captured

        buffer <- withCString "closure_buf" (buildAlloca builder closureType)

        let storeClosure (i, name) = do
            let Just (CompiledValue val _) = lookupEnv env name
            ptr <- getelementptrInt builder buffer [0, i] (name ++ "_ptr")
            buildStore builder val ptr

        mapM_ storeClosure (zip [0..] freevar)

        let i8ptr = pointerType int8Type (fromIntegral 0)

        closurePtr <- withCString "closure_ptr" (buildBitCast builder buffer i8ptr)
        trampPtr <- allocateTrampoline

        trampolineInit <- withCString "llvm.init.trampoline" (\cname -> addFunction mod cname (funType voidType [i8ptr, i8ptr, i8ptr]))
        trampolineAdjust <- withCString "llvm.adjust.trampoline" (\cname -> addFunction mod cname (funType i8ptr [i8ptr]))

        funPtr <- withCString "fun_ptr" (buildBitCast builder fun i8ptr)

        emitCall builder "" trampolineInit [trampPtr, funPtr, closurePtr]
        tramp <- emitCall builder "trampoline" trampolineAdjust [trampPtr]

        let funPtrType = pointerType (compileType typ) (fromIntegral 0)
        funPtr <- withCString "trampoline_fun" (buildBitCast builder tramp funPtrType)

        return $ CompiledValue funPtr typ
        where
        trampSize = 32
        trampAlign = 4
        allocateTrampoline = do
            trampBuf <- withCString "trampoline_buf" (buildAlloca builder (arrayType int8Type (fromIntegral trampSize)))
            setAlignment trampBuf (fromIntegral trampAlign)
            getelementptrInt builder trampBuf [0, 0] "trampoline_ptr"

compileExp ctx env tenv exp@(Let NonRec decls body _) typ = do
    let Right tenv' = runTC $ do
        (sub, tenv') <- tcDecls tenv NonRec decls

        (sub', t) <- tc tenv' body
        sub'' <- unify sub (t, typ)
        return $ substituteEnv sub'' tenv'

    let (names, vals) = unzip decls
    let ts = [t | Just (Scheme [] t) <- map (flip Map.lookup tenv') names]
    vals' <- mapM (\(e, t) -> compileExp ctx env tenv e t) (zip vals ts)

    let env' = Map.fromList (zip names vals') : env
    compileExp ctx env' tenv' body typ

compileExp ctx@(mod, fun, bb, builder, (modenv, modtenv)) env tenv exp@(Let Rec decls body src) typ = do
    let Right tenv' = runTC $ do
        (sub, tenv') <- tcDecls tenv Rec decls
        (sub', t) <- tc tenv' body
        sub'' <- unify sub' (t, typ)
        return (substituteEnv sub'' tenv')

    let funs = [(name, fun, Maybe.fromJust . flip Map.lookup tenv' $ name) | (name, fun@(Lambda _ _ _)) <- decls]
    funs' <- mapM (\(name, _, (Scheme [] t)) -> createFunction mod name t ExternalLinkage []) funs

    let env' = Map.fromList [(name, CompiledValue val t) | ((name, _, (Scheme [] t)), val) <- zip funs funs'] : env

    mapM_ (def env' tenv') (zip funs funs')

    let funnames = [name | (name, _, _) <- funs]
    let decls' = [decl | decl@(name, _) <- decls, not (name `elem` funnames)]
    let exp' = depAnalysis (Let Rec decls' body src)

    compileExp ctx env' tenv' exp' typ
    where
    def env tenv ((name, (Lambda args body _), (Scheme [] t)), f) =
        defineFunction mod f env tenv name args body t []

compileExp _ _ _ _ _ =
    error "Invalid expression"


createFunction :: ModuleRef -> String -> TypeExp -> Linkage -> [(String, TypeExp)] -> IO ValueRef
createFunction mod name typ@(FunctionType args ret) linkage captured = do
    f <- withCString name (\cname -> addFunction mod cname compiledType)
    setLinkage f (fromLinkage linkage)

    when (not . null $ captured) $ do
        addAttribute (getParam f (fromIntegral 0)) (fromAttribute NestAttribute)

    return f
    where
    closureType = (pointerType (struct . map (compileType . snd) $ captured) (fromIntegral 0))
    compiledType
        | null captured = compileType typ
        | otherwise =  funType (compileType ret) (closureType : map compileType args)

compileFunction :: ModuleRef -> CompilerEnv -> TypeEnv -> String -> [Symbol] -> Expression -> TypeExp -> [(String, TypeExp)] -> IO ValueRef
compileFunction mod env tenv name args exp typ@(FunctionType argt ret) captured = do
    f <- createFunction mod name typ ExternalLinkage captured
    defineFunction mod f env tenv name args exp typ captured

defineFunction :: ModuleRef -> ValueRef -> CompilerEnv -> TypeEnv -> String -> [Symbol] -> Expression -> TypeExp -> [(String, TypeExp)] -> IO ValueRef
defineFunction mod f env tenv name args exp typ@(FunctionType argt ret) captured = do
    let firstArg = if null captured then 0 else 1
    let frame = Map.fromList [(sym, CompiledValue (getParam f (fromIntegral i)) typ) | (i, sym, typ) <- zip3 [firstArg..] args argt]
    let frame' = Map.fromList [(name, CompiledValue f typ)]

    let tframe = Map.fromList [(sym, Scheme [] texp) | (sym, texp) <- zip args argt]
    let tframe' = Map.fromList [(name, Scheme [] typ)]

    let nameParameter (i, name) = withCString name $ setValueName (getParam f (fromIntegral i))
    mapM_ nameParameter (zip [firstArg..] args)

    bb <- withCString "" (appendBasicBlock f)
    end <- withCString "end" (appendBasicBlock f)

    builder <- createBuilder
    positionAtEnd builder bb

    let closuret = Map.fromList [(sym, Scheme [] texp) | (sym, texp) <- captured]

    let extractClosure (i, (name, texp)) = do
        let ptr = getelementptrConstInt (getParam f (fromIntegral 0)) [0, i]
        val <- withCString name (buildLoad builder ptr)
        return $ CompiledValue val texp

    closurevals <- mapM extractClosure (zip [0..] captured)
    let closure = Map.fromList $ zip (map fst captured) closurevals

    let env' = frame : closure : frame' : env
    let tenv' = tframe `Map.union` closuret `Map.union` tframe' `Map.union` tenv

    (CompiledValue v _) <- compileExp (mod, f, end, builder, (env, tenv)) env' tenv' exp ret

    buildBr builder end
    positionAtEnd builder end
    ret <- buildRet builder v

    return f

compileModule :: CompilerEnv -> String -> [(Symbol, (Expression, TypeScheme))] -> IO ModuleRef
compileModule =
    undefined

builtinIOp (buildfun, name) (_, _, _, builder, _) env tenv [l@(CompiledValue v1 t1), r@(CompiledValue v2 t2)] texp = do
    val <- withCString name (buildfun builder v1 v2)
    return $ CompiledValue val texp

builtinPlus = builtinIOp (buildAdd, "add")
builtinMinus = builtinIOp (buildSub, "sub")
builtinMul = builtinIOp (buildMul, "mul")

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
builtinEq (_, _, _, builder, _) env tenv [l@(CompiledValue v1 t1), r@(CompiledValue v2 t2)] texp = do
    let name = "eq"
    val <- withCString name (buildICmp builder (fromIntegral 32) v1 v2)
    return $ CompiledValue val texp

builtInTypeEnv = Map.fromList [
    ("+", Scheme [] (FunctionType [Constructor "Int" [], Constructor "Int" []] (Constructor "Int" []))),
    ("-", Scheme [] (FunctionType [Constructor "Int" [], Constructor "Int" []] (Constructor "Int" []))),
    ("*", Scheme [] (FunctionType [Constructor "Int" [], Constructor "Int" []] (Constructor "Int" []))),
    ("eq", Scheme [] (FunctionType [Constructor "Int" [], Constructor "Int" []] (Constructor "Bool" [])))]
builtInCompilerEnv = [Map.fromList [
    ("+", CompiledBuiltIn builtinPlus),
    ("-", CompiledBuiltIn builtinMinus),
    ("*", CompiledBuiltIn builtinMul),
    ("eq", CompiledBuiltIn builtinEq)
    ]]

builtInSymbols = Set.unions (map Map.keysSet builtInCompilerEnv)
