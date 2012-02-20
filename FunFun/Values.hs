module FunFun.Values where

data AtomValue =
    IntValue Integer |
    FloatValue Double |
    StringValue String
    deriving (Eq, Ord, Show)
