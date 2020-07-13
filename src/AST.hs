module AST where

data SrcInfo = SrcInfo
    { lineNum :: Int
    , charNum :: Int
    }
    deriving (Show, Eq, Ord)

data Term
    = TermTrue
    | TermFalse
    | TermIf Term Term Term
    | TermZero
    | TermSucc Term
    | TermPred Term
    | TermIsZero Term
    deriving (Show, Eq, Ord)
