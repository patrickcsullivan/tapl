module TypeChecker where

import           AST

getTypeFromCtx :: Ctx -> Int -> Ty
getTypeFromCtx = (!!)

addBinding :: Ctx -> Ty -> Ctx
addBinding ctx ty = ty : ctx

typeOf :: Ctx -> Term -> Ty
typeOf _   TermTrue          = TyBool
typeOf _   TermFalse         = TyBool
typeOf ctx (TermIf t1 t2 t3) = if typeOf ctx t1 == TyBool
  then
    let tyT2 = typeOf ctx t2
        tyT3 = typeOf ctx t3
    in  if tyT2 == tyT3
          then tyT2
          else error "arms of conditional have different types"
  else error "guard of conditional not a boolean"
typeOf _   TermZero      = TyNat
typeOf ctx (TermSucc t1) = if typeOf ctx t1 == TyNat
  then TyNat
  else error "succ expects an arg of type Nat"
typeOf ctx (TermPred t1) = if typeOf ctx t1 == TyNat
  then TyNat
  else error "pred expects an arg of type Nat"
typeOf ctx (TermIsZero t1) = if typeOf ctx t1 == TyNat
  then TyBool
  else error "isZero expects an arg of type Nat"
typeOf ctx (TermVar idx ctxLen) = getTypeFromCtx ctx idx
typeOf ctx (TermAbs srcVar tyT1 t2) =
  let ctx' = addBinding ctx tyT1
      tyT2 = typeOf ctx' t2
  in  TyArr tyT1 tyT2
typeOf ctx (TermApp t1 t2) =
  let tyT1 = typeOf ctx t1
      tyT2 = typeOf ctx t2
  in  case tyT1 of
        (TyArr tyT11 tyT12) ->
          if tyT2 == tyT11 then tyT12 else error "param type mismatch"
        _ -> error "arrow type expected"
