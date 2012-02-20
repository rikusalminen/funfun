module FunFun.DependencyAnalysis where

import qualified Data.Set as Set
import qualified Data.Tree as Tree
import qualified Data.Graph as Graph

import FunFun.AST
import FunFun.Parser
import FunFun.Pretty
import Text.Parsec

deps decls = [
        (def, name, Set.toList (Set.intersection names (freeVariables def))) |
            (name, def) <- decls]
    where
    names = Set.fromList $ map fst decls

depAnalysis (Tree.Node (LetRec decls, pos) [body]) =
    foldr interleave (depAnalysis body) sccs
    where
    sccs = Graph.stronglyConnCompR . deps $ decls
    interleave (Graph.AcyclicSCC (def, name, _)) body =
        Tree.Node (Let [(name, def)], pos) [body]
    interleave (Graph.CyclicSCC ds) body =
        Tree.Node (LetRec [(name, def) | (def, name, _) <- ds], pos) [body]
depAnalysis (Tree.Node x exprs) =
    Tree.Node x (map depAnalysis exprs)

testCase =
    ast
    where
    (Right ast) = parse parser "" src
    src = "let a = 1, b = a, c = h b d, d = c, f = g h a, g = f, h = g in x"
    -- src = "let a = 1, b = 2, c = 3 in let d = 4, e = 5 in x"

testi = do
    putStrLn . prettyprint $ testCase
    putStrLn . prettyprint . depAnalysis $ testCase


