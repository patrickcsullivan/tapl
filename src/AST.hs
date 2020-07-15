module AST where

data SrcInfo = SrcInfo
    { lineNum :: Int
    , charNum :: Int
    }
    deriving (Show, Eq)

data Ty
    = TyBool
    | TyNat
    | TyArr Ty Ty
    deriving Eq

instance Show Ty where
  show ty = case ty of
    TyBool        -> "Bool"
    TyNat         -> "Nat"
    TyArr ty1 ty2 -> (show ty1) ++ " -> " ++ (show ty2)

type Ctx = [Ty]

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
    | TermAbs String Ty Term
    | TermApp Term Term
    deriving (Show, Eq)
