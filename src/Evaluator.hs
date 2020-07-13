module Evaluator where

import           AST


isNumericVal :: Term -> Bool
isNumericVal trm = case trm of
  (TermZero   ) -> True
  (TermSucc t1) -> isNumericVal t1
  _             -> False

isVal :: Term -> Bool
isVal trm = case trm of
  (TermTrue )        -> True
  (TermFalse)        -> True
  t | isNumericVal t -> True
  _                  -> False



{- | Small-step evaluation.
-}
evalSmall :: Term -> Term
evalSmall trm = if isVal trm then trm else let t1' = eval1 trm in eval t1'
 where
  eval1 :: Term -> Term
  eval1 trm = case trm of
    (TermIf TermTrue t2 _) -> t2
    (TermIf TermFalse _ t3) -> t3
    (TermIf t1 t2 t3) -> let t1' = eval1 t1 in TermIf t1' t2 t3
    (TermSucc t1) -> let t1' = eval1 t1 in TermSucc t1'
    (TermPred TermZero) -> TermZero
    (TermPred (TermSucc nv1)) | isNumericVal nv1 -> nv1
    (TermPred t1) -> let t1' = eval1 t1 in TermPred t1'
    (TermIsZero TermZero) -> TermTrue
    (TermIsZero (TermSucc nv1)) | isNumericVal nv1 -> TermFalse
    (TermIsZero t1) -> let t1' = eval1 t1 in TermIsZero t1'
    _ -> error "no rule applies"

{- | Big-step evalution. (Could still be optimized more.)
-}
eval :: Term -> Term
eval (TermTrue       ) = TermTrue
eval (TermFalse      ) = TermFalse
eval (TermIf t1 t2 t3) = case eval t1 of
  TermTrue  -> eval t2
  TermFalse -> eval t3
  _         -> error "first arg to if must evaluate to a Boolean"
eval TermZero = TermZero
eval (TermSucc t1) =
  let t1' = eval t1
  in  if isNumericVal t1'
        then (TermSucc t1')
        else error "arg to succ must be a numeric val"
eval (TermPred t1) = case eval t1 of
  TermZero -> TermZero
  (TermSucc u1) ->
    let u1' = eval u1
    in  if isNumericVal u1'
          then u1'
          else error "arg to succ must be a numeric val"
  _ -> error "arg to pred must be a numeric val"
eval (TermIsZero t1) = case eval t1 of
  TermZero           -> TermTrue
  t | isNumericVal t -> TermFalse
  _                  -> error "arg to isZero must be a numeric val"


