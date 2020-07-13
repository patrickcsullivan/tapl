module AST where

data SrcInfo = SrcInfo
    { lineNum :: Int
    , charNum :: Int
    }
    deriving (Show, Eq)

data Term
    -- Arithetic
    = TermTrue
    | TermFalse
    | TermIf Term Term Term
    | TermZero
    | TermSucc Term
    | TermPred Term
    | TermIsZero Term
    -- Lambda calculus
    | TermVar Int Int
    | TermAbs String Term
    | TermApp Term Term
    deriving (Show, Eq)
