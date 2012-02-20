module FunFun.Pretty where

import Text.PrettyPrint

import qualified Data.Tree as Tree

import FunFun.AST
import FunFun.Values

pretty (Tree.Node (Identifier sym, _) []) =
    text sym
pretty (Tree.Node (Constant (IntValue x), _) []) = text . show $ x
pretty (Tree.Node (Constant (FloatValue x), _) []) = text . show $ x
pretty (Tree.Node (Constant (StringValue x), _) []) = text . show $ x
pretty (Tree.Node (Application, _) params) =
    sep . map prettyParam $ params
    where
    prettyParam (Tree.Node (Identifier sym, _) []) = text sym
    prettyParam p@(Tree.Node ((Constant _), _) []) = pretty p
    prettyParam param = parens . pretty $ param
pretty (Tree.Node (Lambda sym, _) [x]) =
    text "lambda" <+> text sym <+> text ": " <+> pretty x
pretty (Tree.Node (Conditional, _) [cond, cons, alt]) =
    text "if" <+> pretty cond $$ nest 4 (text "then" <+> pretty cons $$ text "else" <+> pretty alt)
pretty (Tree.Node (LetRec decls, _) [x]) =
    text "let" $$
        nest 4 (vcat (punctuate (text ",") (map prettyDecl decls))) $$
            text "in" $+$
                nest 4 (pretty x)
    where
    prettyDecl (sym, ast) =
        text sym <+> text "=" <+> pretty ast
pretty (Tree.Node (Let decls, pos) xs) =
    pretty (Tree.Node (LetRec decls, pos) xs)
pretty (Tree.Node (DoExpression, _) xs) =
    text "do" $$ nest 4 (vcat (punctuate (text ";") (map pretty xs))) $$ text "done"

prettyprint = render . pretty
