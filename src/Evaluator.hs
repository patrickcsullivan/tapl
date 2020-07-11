module Evaluator where

import           AST

noInfo :: SrcInfo
noInfo = SrcInfo { lineNum = -1, charNum = -1 }

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
evalSmall :: Term -> Term
evalSmall trm = if isVal trm then trm else let t1' = eval1 trm in eval t1'
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

{- | Big-step evalution. (Could still be optimized more.)
-}
eval :: Term -> Term
eval (TermTrue  si     ) = (TermTrue si)
eval (TermFalse si     ) = (TermFalse si)
eval (TermIf _ t1 t2 t3) = case eval t1 of
  (TermTrue  _) -> eval t2
  (TermFalse _) -> eval t3
  _             -> error "first arg to if must evaluate to a Boolean"
eval (TermZero si) = (TermZero si)
eval (TermSucc si t1) =
  let t1' = eval t1
  in  if isNumericVal t1'
        then (TermSucc si t1')
        else error "arg to succ must be a numeric val"
eval (TermPred si t1) = case eval t1 of
  (TermZero si) -> (TermZero si)
  (TermSucc _ u1) ->
    let u1' = eval u1
    in  if isNumericVal u1'
          then u1'
          else error "arg to succ must be a numeric val"
  _ -> error "arg to pred must be a numeric val"
eval (TermIsZero si t1) = case eval t1 of
  (TermZero _)       -> (TermTrue noInfo)
  t | isNumericVal t -> (TermFalse noInfo)
  _                  -> error "arg to isZero must be a numeric val"


