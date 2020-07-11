module AST where

data SrcInfo = SrcInfo
    { lineNum :: Int
    , charNum :: Int
    }
    deriving (Show, Eq, Ord)

data Term
    = TermTrue SrcInfo
    | TermFalse SrcInfo
    | TermIf SrcInfo Term Term Term
    | TermZero SrcInfo
    | TermSucc SrcInfo Term
    | TermPred SrcInfo Term
    | TermIsZero SrcInfo Term
    deriving (Show, Eq, Ord)
