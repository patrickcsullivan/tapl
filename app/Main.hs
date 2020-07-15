module Main where

import           AST
import           Evaluator
import           TypeChecker

main :: IO ()
main = do
  putStrLn $ "f :: " ++ (show $ typeOf [] f)
  putStrLn $ "g :: " ++ (show $ typeOf [] g)
  putStrLn $ "plusOne :: " ++ (show $ typeOf [] plusOne)
  putStrLn $ "plusTwo :: " ++ (show $ typeOf [] plusTwo)
  putStrLn $ "trm :: " ++ (show $ typeOf [] trm)
  putStrLn $ "OUTPUT: " ++ (show $ evalSmall [] trm)

-- Returns +2 if given True and returns +1 if given False.
f = TermAbs "x" TyBool (TermIf (TermVar 0 1) plusTwo plusOne)

-- Function wrapping the built-in isZero term.
g = TermAbs "x" TyNat (TermIsZero (TermVar 0 1))

plusOne = TermAbs "x" TyNat (TermSucc (TermVar 0 2))

plusTwo = TermAbs "x" TyNat (TermSucc (TermSucc (TermVar 0 2)))

trm = TermApp (TermApp f (TermApp g TermZero)) (TermSucc TermZero)
