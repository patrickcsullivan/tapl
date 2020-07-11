module AST where

data SrcInfo = SrcInfo
    { lineNum :: Int
    , charNum :: Int
    }
    deriving (Show, Eq, Ord)

noInfo :: SrcInfo
noInfo = SrcInfo { lineNum = -1, charNum = -1 }

data Term
    = TermTrue SrcInfo
    | TermFalse SrcInfo
    | TermIf SrcInfo Term Term Term
    | TermZero SrcInfo
    | TermSucc SrcInfo Term
    | TermPred SrcInfo Term
    | TermIsZero SrcInfo Term
    deriving (Show, Eq, Ord)

isNumericVal :: Term -> Bool
isNumericVal trm = case trm of
  (TermZero _   ) -> True
  (TermSucc _ t1) -> isNumericVal t1
  _               -> False

isVal :: Term -> Bool
isVal trm = case trm of
  (TermTrue  _)      -> True
  (TermFalse _)      -> True
  t | isNumericVal t -> True
  _                  -> False

{- | Small-step evaluation.
-}
eval :: Term -> Term
eval trm = if isVal trm then trm else let t1' = eval1 trm in eval t1'
 where
  eval1 :: Term -> Term
  eval1 trm = case trm of
    (TermIf _ (TermTrue _) t2 _) -> t2
    (TermIf _ (TermFalse _) _ t3) -> t3
    (TermIf si t1 t2 t3) -> let t1' = eval1 t1 in TermIf si t1' t2 t3
    (TermSucc si t1) -> let t1' = eval1 t1 in TermSucc si t1'
    (TermPred _ (TermZero _)) -> TermZero noInfo
    (TermPred _ (TermSucc _ nv1)) | isNumericVal nv1 -> nv1
    (TermPred si t1) -> let t1' = eval1 t1 in TermPred si t1'
    (TermIsZero _ (TermZero _)) -> TermTrue noInfo
    (TermIsZero _ (TermSucc _ nv1)) | isNumericVal nv1 -> TermFalse noInfo
    (TermIsZero si t1) -> let t1' = eval1 t1 in TermIsZero si t1'
    _ -> error "no rule applies"
