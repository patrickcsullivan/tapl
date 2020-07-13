module Evaluator where

import           AST

type Ctx = [()]

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
  (TermAbs _ _)      -> True
  _                  -> False

--------------------------------------------------------------------------------

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
    TermZero                -> TermZero
    TermSucc   t1           -> let t1' = termMap' absDepth t1 in TermSucc t1'
    TermPred   t1           -> let t1' = termMap' absDepth t1 in TermPred t1'
    TermIsZero t1           -> let t1' = termMap' absDepth t1 in TermIsZero t1'
    -- Lambda calculus
    (TermVar idx    ctxLen) -> onVar absDepth idx ctxLen
    (TermAbs srcVar t1    ) -> TermAbs srcVar $ termMap' (absDepth + 1) t1
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

-- {- | Shift variables in the term with idices greater than or equal to the given
-- cutoff by the given distance.
-- -}
-- shiftVars' :: Int -> Int -> Term -> Term
-- shiftVars' dist cutoff trm = case trm of
--   (TermVar idx ctxLen) -> if idx >= cutoff
--     then TermVar (idx + dist) (ctxLen + dist)
--     else TermVar idx (ctxLen + dist)
--   (TermAbs srcVar t1) -> TermAbs srcVar $ shiftVars' dist (cutoff + 1) t1
--   (TermApp t1 t2) ->
--     let t1' = shiftVars' dist cutoff t1
--         t2' = shiftVars' dist cutoff t2
--     in  TermApp t1' t2'
--   -- TODO: What about arithmetic terms?

-- {- | Shift variables in the term by the given distance.
-- -}
-- shiftVars :: Int -> Term -> Term
-- shiftVars dist trm = shiftVars' dist 0 trm

-- {- | Substitute instances of the specified variable with the given substitution
-- term. The substitution occurs within the specified number of abstraction; this
-- "abstraction depth" is used to properly shift variables in the abstraction body.
-- -}
-- substituteTerm' :: Int -> Int -> Term -> Term -> Term
-- substituteTerm' idx absDepth sub trm = case trm of
--   (TermVar i ctxLen) ->
--     -- Use the abstraction depth to look for the correct index and to shift the
--     -- variables in the substitution term.
--     if i == idx + absDepth then shiftVars absDepth sub else TermVar i ctxLen
--   (TermAbs srcVar t1) ->
--     TermAbs srcVar $ substituteTerm' idx (absDepth + 1) sub t1
--   (TermApp t1 t2) ->
--     let t1' = substituteTerm' idx absDepth sub t1
--         t2' = substituteTerm' idx absDepth sub t2
--     in  TermApp t1' t2'

-- {- | Substitute instances of the specified variable with the given substitution
-- term.
-- -}
-- substituteTerm :: Int -> Term -> Term -> Term
-- substituteTerm idx sub trm = substituteTerm' idx 0 sub trm


--------------------------------------------------------------------------------

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
    (TermApp (TermAbs _ t12) v2) | isVal ctx v2 -> substituteTopTerm v2 t12
    (TermApp v1 t2) | isVal ctx v1 -> let t2' = eval1 ctx t2 in TermApp v1 t2'
    (TermApp t1 t2) -> let t1' = eval1 ctx t1 in TermApp t1' t2
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


