module Main where

import           AST
import           Evaluator

main :: IO ()
main = putStrLn $ show (eval trm)
 where
  trm = TermIf
    noInfo
    (TermIsZero noInfo (TermPred noInfo (TermZero noInfo)))
    (TermSucc noInfo (TermPred noInfo (TermSucc noInfo (TermZero noInfo))))
    (TermTrue noInfo)

out = TermSucc (SrcInfo { lineNum = -1, charNum = -1 })
               (TermZero (SrcInfo { lineNum = -1, charNum = -1 }))
