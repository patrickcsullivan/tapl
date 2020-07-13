module Main where

import           AST
import           Evaluator

main :: IO ()
main = putStrLn $ show (evalSmall [] trm)

f = TermAbs "x" (TermIf (TermVar 0 1) plusTwo plusOne)

g = TermAbs "x" (TermIsZero (TermVar 0 1))

plusOne = TermAbs "x" (TermSucc (TermVar 0 2))

plusTwo = TermAbs "x" (TermSucc (TermSucc (TermVar 0 2)))

trm = TermApp (TermApp f (TermApp g TermZero)) (TermSucc TermZero)
