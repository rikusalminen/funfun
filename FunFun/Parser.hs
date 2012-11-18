module FunFun.Parser (
    parser
    ) where

import qualified Data.Set as Set

import Text.Parsec
import Text.Parsec.Expr

import FunFun.Language
import FunFun.Values
import FunFun.Types
import FunFun.AST

constExpr = do
    pos <- getPosition
    val <- fmap StringValue stringLiteral <|> fmap toConst naturalOrFloat
    return $ Constant val pos
    where
    toConst (Left x) = IntValue x
    toConst (Right y) = FloatValue y

declarations =
    commaSep1 $ do
        pos <- getPosition
        ids <- many1 identifier
        reservedOp "="
        val <- expr
        case ids of
          [sym] -> return (sym, val)
          (name:params) -> return $ (name, Lambda params val pos)

letExpr = do
    pos <- getPosition
    reserved "let"
    binds <- declarations
    reserved "in"
    ex <- expr
    return $ Let Rec binds ex pos

ifExpr = do
    pos <- getPosition
    reserved "if"
    conditions <- sepBy1 conditional (reserved "elif")
    reserved "else"
    alt <- expr
    return $ elifDesugar alt conditions
    where
    elifDesugar =
        foldr (\(cond, cons, pos) alt -> Conditional cond cons alt pos)
    conditional = do
        pos <- getPosition
        l <- expr
        reserved "then"
        r <- expr
        return (l, r, pos)

lambdaExpr = do
    pos <- getPosition
    reserved "lambda"
    syms <- many identifier
    reservedOp ":"
    ex <- expr
    return $ Lambda syms ex pos

basicExpr =
    ident <|>
    constExpr <|>
    letExpr <|>
    ifExpr <|>
    lambdaExpr <|>
    parens expr
    where
    ident = do
        pos <- getPosition
        sym <- identifier
        return $ Variable sym pos

applicationExpr = do
    args <- many1 basicExpr
    case args of
        [x] -> return x
        (fun:args) -> return $ Application fun args (sourcePos fun)

typeVar =
    ((natural >>= return . show) <|> identifier) >>= return . TypeVar

typeConstructor = do
    constructor <- typename
    types <- many typeArg
    return $ Constructor constructor types
    where
    typeArg =
        (do { name <- typename ; return $ Constructor name [] }) <|>
        parens typeConstructor <|>
        typeVar

typeFunction = do
    args <- parens . commaSep1 $ typeExp
    reservedOp "->"
    ret <- typeExp
    return $ FunctionType args ret

typeExp = typeVar <|> typeFunction <|> typeConstructor

typeScheme = do
    exp <- typeExp
    return $ Scheme (Set.toList (typeVarsInExp exp)) exp

typeDeclTail = do
    pos <- getPosition
    reservedOp "::"
    scheme <- typeScheme
    return $ \expr -> (TypeDecl scheme expr pos)

opExpr =
    buildExpressionParser opTable applicationExpr
    where
    opTable = [
        [Infix (inf "*") AssocLeft],
        [Infix (inf "+") AssocLeft, Infix (inf "-") AssocLeft],
        [Postfix typeDeclTail]
        ]
    inf op = do
        pos <- getPosition
        reservedOp op
        return $ \l r -> Application (Variable op pos) [l , r] pos


expr = opExpr

parser = do
    ex <- expr
    eof
    return ex
