module Evaluator where

import           AST

isNumericVal :: Term -> Bool
isNumericVal trm = case trm of
  (TermZero   ) -> True
  (TermSucc t1) -> isNumericVal t1
  _             -> False

isVal :: Ctx -> Term -> Bool
isVal _ trm = case trm of
  (TermTrue )        -> True
  (TermFalse)        -> True
  t | isNumericVal t -> True
  (TermAbs _ _ _)    -> True
  _                  -> False

termMap :: (Int -> Int -> Int -> Term) -> Term -> Term
termMap onVar trm = termMap' 0 trm
 where
  termMap' absDepth t = case t of
    -- Artithmetic
    TermTrue  -> TermTrue
    TermFalse -> TermFalse
    (TermIf t1 t2 t3) ->
      let t1' = termMap' absDepth t1
          t2' = termMap' absDepth t2
          t3' = termMap' absDepth t3
      in  TermIf t1' t2' t3'
    TermZero               -> TermZero
    TermSucc   t1          -> let t1' = termMap' absDepth t1 in TermSucc t1'
    TermPred   t1          -> let t1' = termMap' absDepth t1 in TermPred t1'
    TermIsZero t1          -> let t1' = termMap' absDepth t1 in TermIsZero t1'
    -- Lambda calculus
    (TermVar idx ctxLen  ) -> onVar absDepth idx ctxLen
    (TermAbs srcVar ty t2) -> TermAbs srcVar ty $ termMap' (absDepth + 1) t2
    (TermApp t1 t2) ->
      let t1' = termMap' absDepth t1
          t2' = termMap' absDepth t2
      in  TermApp t1' t2'

{- | Shift variables in the term by the given distance.
-}
shiftVars :: Int -> Term -> Term
shiftVars dist = termMap
  (\absDepth idx ctxLen -> if idx >= absDepth
    then TermVar (idx + dist) (ctxLen + dist)
    else TermVar idx (ctxLen + dist)
  )

{- | Substitute instances of the specified variable with the given substitution
term.
-}
substituteTerm :: Int -> Term -> Term -> Term
substituteTerm idx sub = termMap
  (\absDepth i ctxLen ->
    -- Use the abstraction depth to look for the correct index and to shift the
    -- variables in the substitution term.
    if i == idx + absDepth then shiftVars absDepth sub else TermVar i ctxLen
  )

{- | Substitute instances of the 0-index variable with the given term.
-}
substituteTopTerm :: Term -> Term -> Term
substituteTopTerm sub trm =
  shiftVars (-1) $ substituteTerm 0 (shiftVars 1 sub) trm

{- | Small-step evaluation.
-}
evalSmall :: Ctx -> Term -> Term
evalSmall _ trm = if isVal [] trm
  then trm
  else let t1' = eval1 [] trm in evalSmall [] t1'
 where
  eval1 :: Ctx -> Term -> Term
  eval1 ctx trm = case trm of
    -- Arithmetic
    (TermIf TermTrue t2 _) -> t2
    (TermIf TermFalse _ t3) -> t3
    (TermIf t1 t2 t3) -> let t1' = eval1 ctx t1 in TermIf t1' t2 t3
    (TermSucc t1) -> let t1' = eval1 ctx t1 in TermSucc t1'
    (TermPred TermZero) -> TermZero
    (TermPred (TermSucc nv1)) | isNumericVal nv1 -> nv1
    (TermPred t1) -> let t1' = eval1 ctx t1 in TermPred t1'
    (TermIsZero TermZero) -> TermTrue
    (TermIsZero (TermSucc nv1)) | isNumericVal nv1 -> TermFalse
    (TermIsZero t1) -> let t1' = eval1 ctx t1 in TermIsZero t1'
    -- Lambda Calculus
    (TermApp (TermAbs _ _ t12) v2) | isVal ctx v2 -> substituteTopTerm v2 t12
    (TermApp v1 t2) | isVal ctx v1 -> let t2' = eval1 ctx t2 in TermApp v1 t2'
    (TermApp t1 t2) -> let t1' = eval1 ctx t1 in TermApp t1' t2
    _ -> error "no rule applies"

{- | Big-step evalution. (Could still be optimized more.)
-}
eval :: Ctx -> Term -> Term
-- Arithmetic
eval ctx (TermTrue       ) = TermTrue
eval ctx (TermFalse      ) = TermFalse
eval ctx (TermIf t1 t2 t3) = case eval ctx t1 of
  TermTrue  -> eval ctx t2
  TermFalse -> eval ctx t3
  _         -> error "first arg to if must evaluate to a Boolean"
eval ctx TermZero = TermZero
eval ctx (TermSucc t1) =
  let t1' = eval ctx t1
  in  if isNumericVal t1'
        then (TermSucc t1')
        else error "arg to succ must be a numeric val"
eval ctx (TermPred t1) = case eval ctx t1 of
  TermZero -> TermZero
  (TermSucc u1) ->
    let u1' = eval ctx u1
    in  if isNumericVal u1'
          then u1'
          else error "arg to succ must be a numeric val"
  _ -> error "arg to pred must be a numeric val"
eval ctx (TermIsZero t1) = case eval ctx t1 of
  TermZero           -> TermTrue
  t | isNumericVal t -> TermFalse
  _                  -> error "arg to isZero must be a numeric val"
-- Lambda calculus
eval ctx (TermApp t1 t2) =
  let t1' = eval ctx t1
      t2' = eval ctx t2
  in  case t1' of
        (TermAbs _ _ t12) -> eval ctx $ substituteTopTerm t2' t12
        _ -> error "an arg can only be applied to an abstraction"
-- Values
eval ctx t = if isVal ctx t then t else error $ "no rule applies to " ++ show t
