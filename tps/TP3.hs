-- Build me with: cabal build TP3.hs
-- Execute me with: cabal run -v0 TP3.hs
-- Load me in the REPL with: cabal repl TP3.hs, then use :r to reload the code upon changing

{- HLINT ignore -}

module Main where

import Debug.Trace
import GHC.Generics
import Generic.Random
import Test.QuickCheck
import Network.HTTP.Req (Url)
import Data.List.Split
import Data.List

main :: IO ()
main = do
  putStrLn "TP3 is running"
  putStrLn "--------------"
  putStrLn "Passing tests:"
  putStrLn $ "Testing checkURLParsing on 'http://www.google.fr/': " ++ show (checkURLParsing "http://www.google.fr/")
  putStrLn $ "Testing checkURLParsing on 'https://github.com/dmjio/miso/pulls': " ++ show (checkURLParsing "https://github.com/dmjio/miso/pulls")
  putStrLn $ "Testing checkURLParsing on 'http://reddit.com/r/haskell': " ++ show (checkURLParsing "http://reddit.com/r/haskell")
  putStrLn "--------------"
  putStrLn "Failing tests:"
  putStrLn $ "Testing checkURLParsing on 'htp://www.google.fr/': " ++ show (checkURLParsing "htp://www.google.fr/")
  putStrLn $ "Testing checkURLParsing on 'https://github.co/dmjio/miso/pulls': " ++ show (checkURLParsing "https://github.co/dmjio/miso/pulls")
  putStrLn $ "Testing checkURLParsing on 'http://redditcom/r/haskell': " ++ show (checkURLParsing "http://redditcom/r/haskell")


-- 1/ Define a type representing URLs you can enter in a browser address bar,
--    i.e. strings of the form:
--    - http://www.google.fr
--    - https://github.com/dmjio/miso/pulls
--    - http://reddit.com/r/haskell
-- 2/ Write a parser from String to your URL type. Its return type must be
--    Either String MyType. The 'Left' case is the error message. Write tests.
-- 3/ We now want to do URL filtering, to grant/forbid access to an URL
--    based on a policy. A filter is a string of the form:
--    - lemonde.fr/emploi
--    - reddit.com/**
--    - reddit.com/*/haskell/**
--    The second filter rules out all URLs of the form reddit.com/suffix, for any 'suffix'
--    The third filter rules out all URLs of the form reddit.com/whatever/haskell/suffix,
--    for any 'whatever' and any 'suffix'. In other words, '*' matches any single
--    segment of the URL while '**' matches all possibles suffixes.
--
--    Write a type for filters
-- 4/ Write a function that taskes an URL and a filter, and returns whether
--    the URL passes the filter. Write tests.
--
-- Use https://hoogle.haskell.org/ to find the functions you need, for example splitOn:
-- https://hackage.haskell.org/package/split-0.2.5/docs/Data-List-Split.html#v:splitOn

data Protocol = HTTP | HTTPS
data Extension = FR | COM

instance Show Protocol where
  show HTTP = "http"
  show HTTPS = "https"

instance Show Extension where
  show FR = "fr"
  show COM = "com"

data URL = DefURL {
  protocol :: Protocol,
  subdomains :: [String],
  domain :: String,
  extension :: Extension,
  path :: [String]
}

instance Show URL where
  show (DefURL protocol subdomains domain extension path) = show protocol ++ "://" ++ formatSubdomains subdomains ++ domain ++ "." ++ show extension ++ "/" ++ intercalate "/" path


formatSubdomains :: [String] -> String
formatSubdomains subdomains = case subdomains of 
  [] -> ""
  l -> intercalate "." l ++ "."

parseURL :: String -> Either String URL
parseURL url = case (splitOn "://" url) of
  [protocol, rest] -> case parseProtocol protocol of
    Right p -> parseRightURL rest p
    Left m -> Left m
  _ -> Left "Error: bad format for URL"

parseProtocol :: String -> Either String Protocol
parseProtocol "http" = Right HTTP
parseProtocol "https" = Right HTTPS
parseProtocol _ = Left "Error: bad format for protocol"

parseExtension :: String -> Either String Extension
parseExtension "fr" = Right FR
parseExtension "com" = Right COM
parseExtension _ = Left "Error: bad format for extension"

parseRightURL :: String -> Protocol -> Either String URL
parseRightURL url protocol = case (splitOn "/" url) of
  (domains : path) -> parseDomains domains protocol path
  _ -> Left "Error: bad format for URL"

parseDomains :: String -> Protocol -> [String] -> Either String URL
parseDomains domains protocol path = case reverse (splitOn "." domains) of
  (extension : domain : subdomainsRev) -> case parseExtension extension of
    Right e -> Right (DefURL { protocol = protocol, subdomains = reverse subdomainsRev, domain = domain, extension = e, path = path })
    Left m -> Left m
  _ -> Left "Error: bad format for URL"

checkURLParsing :: String -> Bool
checkURLParsing url = case parseURL url of
  Right urlParsed -> url == show urlParsed
  Left _ -> False

data PathFilter = SingleWhiteCard | AnyWhiteCard | Str String
data Filter = DefFilter {
  domain :: String,
  extension :: Extension,
  path :: [PathFilter]
}