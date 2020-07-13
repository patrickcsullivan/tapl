module Main where

import           AST
import           Evaluator

main :: IO ()
main = putStrLn $ show (evalSmall [] trm)

f =
  TermAbs "x" (TermIf (TermVar 0 1) (TermSucc (TermSucc (TermZero))) (TermZero))

g = TermAbs "x" (TermIsZero (TermVar 0 1))

trm = TermApp f (TermApp g TermZero)
