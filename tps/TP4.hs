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
import Test.QuickCheck.Monadic
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
  putStrLn "MANUAL TESTS"
  putStrLn "--------------"
  let t1 = "1+1"
  let t2 = "1-2"
  let t3 = "1+2+3+4+5"
  let t4 = "1+2-3+4-5"
  let t5 = "12-13+14"
  let t6 = "1-2-3-4-5-6+7"
  let t7 = "-5+5"
  let t8 = "--5"
  let t9 = "1--5"
  let t10 = "-8+8"
  let t11 = "--7+5"
  putStrLn (showTest t1)
  putStrLn (showTest t2)
  putStrLn (showTest t3)
  putStrLn (showTest t4)
  putStrLn (showTest t5)
  putStrLn (showTest t6)
  putStrLn (showTest t7)
  putStrLn (showTest t8)
  putStrLn (showTest t9)
  putStrLn (showTest t10)
  putStrLn (showTest t11)
  putStrLn "--------------"
  putStrLn "QUICKCHECK TESTS"
  putStrLn "--------------"
  quickCheck prop_eval_matches_python


data Expr = Constant Int | Add Expr Expr | Neg Expr

newtype ExprString = ExprString { getExprString :: String }

instance Show ExprString where
  show = getExprString

instance Arbitrary ExprString where
  arbitrary = ExprString <$> sized genExprString
    where
      genDigits :: Gen String
      genDigits = do
        n <- choose (0, 1000 :: Int)
        return (show n)

      genExprString :: Int -> Gen String
      genExprString 0 = genDigits
      genExprString n = do
        k <- choose (1, max 1 (n `div` 2))
        parts <- vectorOf k genDigits
        ops <- vectorOf (k - 1) $ do
          op <- elements ['+', '-']
          unaryNeg <- arbitrary
          return (op, unaryNeg)
        return (build parts ops)

      build :: [String] -> [(Char, Bool)] -> String
      build [] _ = ""
      build (p:ps) ops = p ++ concat (zipWith combine ops ps)
        where
          combine (op, un) q = op : (if un then '-' : q else q)

prop_eval_matches_python :: ExprString -> Property
prop_eval_matches_python s = monadicIO $ do
  let expr = parse s.getExprString
  pyResult <- run (pyEval s.getExprString)
  let expected = read pyResult
  let actual = eval expr
  assert (actual == expected)


showTest :: String -> String
showTest test = "Parsing '" ++ test ++ "' gives: '" ++ show (parse test) ++ "' and is evaluated to: '" ++ show (eval (parse test)) ++ "'"

eval :: Expr -> Int
eval (Constant c) = c
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Neg e) = - eval e

instance Show Expr where
  show (Constant c) = show c
  -- show (Add e1 (Add (Neg e2) e3)) = show e1 ++ "-" ++ show e2 ++ "+" ++ show e3
  -- show (Add e1 (Neg e2)) = show e1 ++ "-" ++ show e2
  show (Add e1 e2) = show e1 ++ "+" ++ show e2
  show (Neg e) = "(-" ++ show e ++ ")"



parse :: String -> Expr
parse s = parseAux s 0 False False

parseAux :: String -> Int -> Bool -> Bool ->Expr
parseAux string current neg hasInt = case string of
  [] | neg -> Neg (Constant current)
  [] -> Constant current
  c : q | isDigit c -> parseAux q (current*10 + digitToInt c) neg True
  '+' : q | neg -> Add (Neg (Constant current)) e where e = parseAux q 0 False False
  '+' : q -> Add (Constant current) e where e = parseAux q 0 False False
  '-' : q | not hasInt -> e where e = parseAux q 0 (not neg) False
  '-' : q | neg -> Add (Neg (Constant current)) e where e = parseAux q 0 True False
  '-' : q -> Add (Constant current) e where e = parseAux q 0 True False
  _ -> Constant 0
-- 12-13+14 Add(12, Add(Neg(13), 14))
-- 1-2+3+4 Add(1, Add(Neg(2), Add))
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