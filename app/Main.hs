module Main where

import           AST
import           Evaluator

main :: IO ()
main = putStrLn $ show (eval trm)
 where
  trm = TermIf (TermIsZero (TermPred TermZero))
               (TermSucc (TermPred (TermSucc TermZero)))
               TermTrue

out = TermSucc TermZero

