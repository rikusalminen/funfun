module FunFun.Pretty where

import Text.PrettyPrint

import FunFun.AST
import FunFun.Values

pretty (Variable sym _) =
    text sym
pretty (Constant (IntValue x) _) = text . show $ x
pretty (Constant (FloatValue x) _) = text . show $ x
pretty (Constant (StringValue x) _) = text . show $ x
pretty (Application left right _) =
    prettyFun left <+> prettyParam right
    where
    prettyFun f@(Application _ _ _) = pretty f
    prettyFun x = prettyParam x
    prettyParam (Variable sym _) = text sym
    prettyParam p@(Constant _ _) = pretty p
    prettyParam param = parens . pretty $ param
pretty (Lambda sym body _) =
    text "lambda" <+> text sym <> text ":" <+> pretty body
pretty (Conditional cond cons alt _) =
    text "if" <+> pretty cond $$ nest 4 (text "then" <+> pretty cons $$ text "else" <+> pretty alt)
pretty (Let rec decls x _) =
    text "let" $$
        nest 4 (vcat (punctuate (text ",") (map prettyDecl decls))) $$
            text "in" $+$
                nest 4 (pretty x)
    where
    prettyDecl (sym, ast) =
        text sym <+> text "=" <+> pretty ast

prettyprint = render . pretty
