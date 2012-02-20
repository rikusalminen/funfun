import Text.Parsec (parse)

import FunFun.Parser
import FunFun.Pretty
import FunFun.Interpreter

test1 =
    test "x+x"

test2 =
    test "if x + y then z + w elif q+w then e+r else t+y"

test3 =
    test "x (y+z) (w q w) (r ty)"

test4 =
    test "let x = x, y = x, z  x y= x + y in x + y + z"

test5 =
    test "do x ; y ; z done"

test6 =
    test "(x + (y + z)) + w"

test7 =
    test "lambda x y : x + y"

test8 =
    test "x + 13 + 15.05"

test9 =
    testInterp "(lambda x: if x then 1 else 0) 13"

test10 =
    testInterp "lambda x y: 13"

test11 =
    testInterp "let x = 1, y = 2 in y"

testInterp src =
    case parse parser "" src of
      Left err -> print err
      Right ast -> do
          putStrLn . prettyprint $ ast
          print (eval [] ast)

test src =
    case parse parser "" src of
      Left err -> print err
      Right ast -> putStr . prettyprint $ ast
