-- Build me with: cabal build TP1.hs
-- Execute me with: cabal run -v0 TP1.hs
-- Load me in the REPL with: cabal repl TP1.hs, then use :r to reload the code upon changing

--
-- In this file, you need to replace the @undefined@ calls by real code.
--
-- Resource for the syntax of functions: https://learnyouahaskell.github.io/syntax-in-functions.html

{- HLINT ignore -}

module Main where

import Debug.Trace
import GHC.Generics
import Generic.Random
import Test.QuickCheck
import Prelude hiding (and, drop, length, not, take, not)

-- | A type for thumb up and thumb down emojis
data ThumbType = Up | Down
  deriving (Show) -- So that the type can be printed

main :: IO ()
main = do
  putStrLn ("Haskell is " ++ (show Up))
  quickCheck propNegAnd -- Tests the property 'propNegAnd'
  quickCheck (propLength :: [Int] -> Bool) -- Specialize function, so that data can be generated
  quickCheck propSumRecSumFold

-- | Write a negation function over Bool: 'neg'
neg :: Bool -> Bool
neg True = False
neg False = True

neg' :: Bool -> Bool
neg' x = case x of
  True -> False
  False -> True

neg'' :: Bool -> Bool
neg'' = \case
  True -> False
  False -> True

neg''' :: Bool -> Bool
neg''' = \x -> case x of
  True -> False
  False -> True

-- | Write the conjunction function over Bool: 'and'
and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False

and' :: Bool -> Bool -> Bool
and' x y = case (x, y) of
  (True, True) -> True
  _ -> False

-- | A function stating a property of 'neg' and 'and'
propNegAnd :: Bool -> Bool
propNegAnd b = neg (and b (neg b))

-- | Write a function computing the length of a list
length :: [a] -> Int
length [] = 0
length (_ : q) = 1 + length q

length' :: [a] -> Int
length' l = foldl (\acc _x -> acc + 1) 0 l

length'' :: [a] -> Int
length'' l = go 0 l
  where
    go :: Int -> [a] -> Int
    go acc [] = acc
    go acc (_x : xs) = go (acc + 1) xs

-- | write a function that states a property of 'length', for any input
-- list.
propLength :: [a] -> Bool
propLength q = length (take n q) + length (drop n q) == length q where n = div (length q) 2

-- | Write a function taking the first 'n' elements of a list. The function
-- should be total.
take :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (a : q) = (a : take (n - 1) q)


-- | Write a function taking the suffix of a list, after the first 'n' elements.
drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 q = q
drop n (_ : q) = drop (n - 1) q

-- | Write a recursive function that sums the elements of a list
sumRec :: [Int] -> Int
sumRec [] = 0
sumRec (a : q) = a + sumRec q

-- | Write a non-recursive function that sums the elements of a list, using
-- the foldr function: https://hoogle.haskell.org/?hoogle=foldr
sumFold :: [Int] -> Int
sumFold q = foldr (+) 0 q

-- | Write a function stating a relation between 'sumRec' and 'sumFold'
propSumRecSumFold :: [Int] -> Bool
propSumRecSumFold q = sumRec q == sumFold q

-- | Write the fmap instance for 'Maybe'
fmapMaybe :: (a -> b) -> (Maybe a) -> (Maybe b)
fmapMaybe _ Nothing = Nothing
fmapMaybe f (Just x) = Just (f x) 

-- | Write the map instance for 'List'. Don't use the standard library's 'map' function
fmapMaybeList :: (a -> b) -> [a] -> [b]
fmapMaybeList _ [] = []
fmapMaybeList f  (x : q) = (f x : fmapMaybeList f q)