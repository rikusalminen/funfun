import Data.Map

import Text.Parsec (parse)

import FunFun.Parser
import FunFun.Pretty
import FunFun.Interpreter
import FunFun.DependencyAnalysis
import FunFun.TypeChecker

testInterp src =
    case parse parser "" src of
        Left err -> print err
        Right ast -> do
            putStrLn . prettyprint $ ast
            let optimized = depAnalysis ast
            putStrLn . prettyprint $ optimized
            -- print (eval [] ast)
            print optimized
            case typeTest optimized of
                Left err -> putStrLn err
                (Right (_, t)) -> print t
            -- print (eval [] optimized)


main = do
    forever (getLine >>= testInterp)
    where
    forever a = do { a ; forever a }
