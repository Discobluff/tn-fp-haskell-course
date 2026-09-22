-- Build me with: cabal build TP4.hs
-- Execute me with: cabal run -v0 TP4.hs
-- Load me in the REPL with: cabal repl TP4.hs, then use :r to reload the code upon changing

{- HLINT ignore -}

module Main where

import Debug.Trace
import GHC.Generics
import Generic.Random
import System.Process
import Test.QuickCheck
import Data.Char (digitToInt)

-- The goal of this TP is to implement an evaluator for arithmetic expressions.
-- Here is the incremental list of objectives:
-- - Define a type of arithmetic expressions. It should support:
--   - Int constants
--   - Additions of two expressions
--   - Negation of an expression
--   - Feel free to add constructs
-- - Define an evaluator for an expression. I.e. given your type of expressions
--   Expr, define the function eval :: Expr -> Int
-- - Make 'Expr' an instance of the 'Show' typeclass
-- - Use this instance to compare your implementation of 'eval' with
--   the result of evaluating your expressions with python3, using the 'pyEval'
--   function below. Write a quickCheck test to compare the two implementations.

-- | pyEval calls python3 and make it execute a python statement.
-- For example, pyEval "2 * 3" returns "6"
-- For 'readProcess' documentation, see:
-- https://hackage.haskell.org/package/process-1.6.13.2/docs/System-Process.html#v:readProcess
--
-- Use 'monadicIO' to use 'pyEval' in a QuickCheck property:
-- https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck-Monadic.html#v:monadicIO
--
-- Use https://hoogle.haskell.org/ to find the functions you need
pyEval :: String -> IO String
pyEval expr = do
  readProcess "python3" ["-c", "print(" ++ expr ++ ", end='')"] ""

main :: IO ()
main = do
  pyResult <- pyEval "1 + 3"
  putStrLn ("pyEval \"1 + 3\" returned: " ++ pyResult)
  putStrLn "--------------"
  putStrLn "HAND TESTS"
  putStrLn "--------------"
  let t1 = "1+1"
  let t2 = "1-2"
  let t3 = "1+2+3+4+5"
  let t4 = "1+2-3+4-5"
  putStrLn (showTest t1)
  putStrLn (showTest t2)
  putStrLn (showTest t3)
  putStrLn (showTest t4)

data Expr = Constant Int | Add Expr Expr | Neg Expr

showTest :: String -> String
showTest test = "Parsing '" ++ test ++ "' gives: '" ++ show (parse test) ++ "' and is evaluated to: '" ++ show (eval (parse test)) ++ "'"

eval :: Expr -> Int
eval (Constant c) = c
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Neg e) = - eval e

instance Show Expr where
  show (Constant c) = show c
  show (Add e1 (Neg e2)) = show e1 ++ "-" ++ show e2
  show (Add e1 e2) = show e1 ++ "+" ++ show e2
  show (Neg e) = "-" ++ show e



parse :: String -> Expr
parse s = parseAux s 0 False

parseAux :: String -> Int -> Bool -> Expr
parseAux string current neg = case string of
  [] | neg -> Neg (Constant current)
  [] -> Constant current
  c : q | isDigit c -> parseAux q (current*10 + digitToInt c) neg
  '+' : q | neg -> Add (Neg (Constant current)) e where e = parseAux q 0 False
  '+' : q -> Add (Constant current) e where e = parseAux q 0 False
  '-' : q -> Add (Constant current) e where e = parseAux q 0 True
  _ -> Constant 0

isDigit :: Char -> Bool
isDigit char = char >= '0' && char <= '9'

-- op1 :: State -> Maybe State
-- op2 :: State -> Maybe State
-- op3 :: State -> Maybe Int
-- op123 :: State -> Maybe Int
-- op123 s = 
--  s' <- op1 s
--  s'' <- op2 s'
--  op3 s''