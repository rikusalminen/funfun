module FunFun.Language (
    identifier,
    reserved,
    reservedOp,
    naturalOrFloat,
    natural,
    stringLiteral,
    parens,
    commaSep,
    commaSep1,
    semiSep1,
    lexeme,
    typename
    ) where

import Text.Parsec
import qualified Text.Parsec.Language as Language
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Char as Char

language = Language.haskellDef {
    Token.reservedNames = ["lambda", "if", "then", "elif", "else", "let", "in", "do", "done"],
    Token.reservedOpNames = ["=", ".", ":"],
    Token.identStart = Char.lower
    }
lexer = Token.makeTokenParser language

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
natural = Token.natural lexer
naturalOrFloat = Token.naturalOrFloat lexer
stringLiteral = Token.stringLiteral lexer
parens = Token.parens lexer
commaSep = Token.commaSep lexer
commaSep1 = Token.commaSep1 lexer
semiSep1 = Token.semiSep1 lexer
lexeme = Token.lexeme lexer
typename = do
    lexeme $ do
    x <- Char.upper
    xs <- many (Token.identLetter language)
    return (x:xs)
