module FunFun.DependencyAnalysis where

import qualified Data.Set as Set
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

depAnalysis (Let Rec decls body pos) =
    foldr interleave (depAnalysis body) sccs
    where
    decls' = [(name, depAnalysis val) | (name, val) <- decls]
    sccs = Graph.stronglyConnCompR . deps $ decls'
    interleave (Graph.AcyclicSCC (def, name, _)) body =
        Let NonRec [(name, def)] body pos
    interleave (Graph.CyclicSCC ds) body =
        Let Rec [(name, def) | (def, name, _) <- ds] body pos
depAnalysis (Let NonRec decls body pos) =
    Let NonRec [(name, depAnalysis val) | (name, val) <- decls] (depAnalysis body) pos
depAnalysis (Application fun args pos) =
    Application (depAnalysis fun) (map depAnalysis args) pos
depAnalysis (Conditional cond cons alt pos) =
    Conditional (depAnalysis cond) (depAnalysis cons) (depAnalysis alt) pos
depAnalysis (Lambda var body pos) =
    Lambda var (depAnalysis body) pos
depAnalysis x = x
