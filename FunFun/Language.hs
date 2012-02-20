module FunFun.Language where

import qualified Text.Parsec.Language as Language
import qualified Text.Parsec.Token as Token

language = Language.haskellDef {
    Token.reservedNames = ["lambda", "if", "then", "elif", "else", "let", "in", "do", "done"],
    Token.reservedOpNames = ["=", ".", ":"]
    }
lexer = Token.makeTokenParser language

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
naturalOrFloat = Token.naturalOrFloat lexer
stringLiteral = Token.stringLiteral lexer
parens = Token.parens lexer
commaSep = Token.commaSep lexer
commaSep1 = Token.commaSep1 lexer
semiSep1 = Token.semiSep1 lexer
