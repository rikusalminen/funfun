module FunFun.Parser where

import Data.Tree as Tree

import Text.Parsec
import Text.Parsec.Expr

import FunFun.Language
import FunFun.Values
import FunFun.AST

lambda pos =
    foldr (\ident expr -> Tree.Node (Lambda ident, pos) [expr])

application :: [AST] -> AST
application exps =
    foldl1 (\l@(Tree.Node (_, pos) _) r -> Tree.Node (Application, pos) [l, r]) exps

constExpr = do
    pos <- getPosition
    val <- fmap StringValue stringLiteral <|> fmap toConst naturalOrFloat
    return $ Tree.Node (Constant val, pos) []
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
          (name:params) -> return $ (name, lambda pos val params)

letExpr = do
    pos <- getPosition
    reserved "let"
    binds <- declarations
    reserved "in"
    ex <- expr
    return $ Tree.Node (LetRec binds, pos) [ex]

ifExpr = do
    pos <- getPosition
    reserved "if"
    conditions <- sepBy1 conditional (reserved "elif")
    reserved "else"
    alt <- expr
    return $ elifDesugar alt conditions
    where
    elifDesugar =
        foldr (\(cond, cons, pos) alt -> Tree.Node (Conditional, pos) [cond, cons, alt])
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
    return $ lambda pos ex syms

doExpr = do
    pos <- getPosition
    reserved "do"
    exprs <- semiSep1 expr
    reserved "done"
    return $ Tree.Node (DoExpression, pos) exprs

basicExpr =
    ident <|>
    constExpr <|>
    letExpr <|>
    ifExpr <|>
    lambdaExpr <|>
    doExpr <|>
    parens expr
    where
    ident = do
        pos <- getPosition
        sym <- identifier
        return $ Tree.Node (Identifier sym, pos) []

opExpr =
    buildExpressionParser opTable basicExpr
    where
    opTable = [
        [Infix (inf "+") AssocLeft]
        ]
    inf op = do
        pos <- getPosition
        reservedOp op
        return $ \l r -> application [Tree.Node (Identifier op, pos) [], l , r]


expr = do
    ex <- many1 opExpr
    case ex of
      [x] -> return x
      xs -> return $ application xs

parser = do
    ex <- expr
    eof
    return ex
