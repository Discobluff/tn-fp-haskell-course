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
  putStrLn "--------------"
  let filter1 = DefFilter [] "lemonde" FR []
  let filter2 = DefFilter [] "reddit" COM [AnyWhiteCard]
  let filter3 = DefFilter [] "reddit" COM [SingleWhiteCard, Str "haskell", AnyWhiteCard]
  putStrLn "TESTS FILTER"
  putStrLn "--------------"
  putStrLn "Passing tests:"
  putStrLn $ "Test accept on 'http://lemonde.fr' with the filter '" ++ show filter1 ++ "': " ++ show (accept filter1 "http://lemonde.fr")
  putStrLn $ "Test accept on 'http://reddit.com.' with the filter '" ++ show filter2 ++ "': " ++ show (accept filter2 "http://reddit.com")
  putStrLn $ "Test accept on 'http://reddit.com/bart/haskell' with the filter '" ++ show filter3 ++ "': " ++ show (accept filter3 "http://reddit.com/bart/haskell")
  putStrLn $ "Test accept on 'http://reddit.com/bart/haskell/marge' with the filter '" ++ show filter3 ++ "': " ++ show (accept filter3 "http://reddit.com/bart/haskell/marge")
  putStrLn "--------------"
  putStrLn "Failing tests:"
  putStrLn $ "Test accept on 'http://leonde.fr' with the filter '" ++ show filter1 ++ "': " ++ show (accept filter1 "http://leonde.fr")
  putStrLn $ "Test accept on 'http://reddit.fr.' with the filter '" ++ show filter2 ++ "': " ++ show (accept filter2 "http://reddit.fr")
  putStrLn $ "Test accept on 'http://reddit.com/bart/hasell' with the filter '" ++ show filter3 ++ "': " ++ show (accept filter3 "http://reddit.com/bart/hasell")
  putStrLn $ "Test accept on 'http://reddit.com/haskell' with the filter '" ++ show filter3 ++ "': " ++ show (accept filter3 "http://reddit.com/haskell")


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
instance Eq Extension where
  FR == FR = True
  COM == COM = True
  _ == _ = False

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
  subdomains :: [String],
  domain :: String,
  extension :: Extension,
  path :: [PathFilter]
}
instance Show PathFilter where
  show SingleWhiteCard = "*"
  show AnyWhiteCard = "**"
  show (Str s) = s

instance Show Filter where
  show filter = (intercalate "." filter.subdomains) ++ filter.domain ++ "." ++ show filter.extension ++ "/" ++ (intercalate "/" (map show filter.path))

accept :: Filter -> String -> Bool
accept filter urlString = case parseURL urlString of
  Right url -> acceptURL filter url
  Left _ -> False

acceptURL :: Filter -> URL -> Bool
acceptURL filter url = (acceptURLBase filter url) && (acceptURLPath filter.path url.path)

acceptURLBase :: Filter -> URL -> Bool
acceptURLBase (DefFilter {subdomains = subdomains1, domain = domain1, extension = extension1}) (DefURL { subdomains = subdomains2, domain = domain2, extension = extension2}) =
  domain1 == domain2 && extension1 == extension2 && subdomains1 == subdomains2

acceptURLPath :: [PathFilter] -> [String] -> Bool
acceptURLPath filter url = case (filter, url) of
  ([], []) -> True
  (AnyWhiteCard : _ , _) -> True
  (SingleWhiteCard : qFilter, _ : qUrl) -> acceptURLPath qFilter qUrl
  ((Str str1) : qFilter, str2 : qUrl) -> str1 == str2 && acceptURLPath qFilter qUrl
  _ -> False
  

checkFormat :: Filter -> Bool
checkFormat filter = checkFormatFilterPath filter.path

checkFormatFilterPath :: [PathFilter] -> Bool
checkFormatFilterPath list = case list of
  [] -> True 
  AnyWhiteCard : [] -> True
  AnyWhiteCard : _ -> False
  _ : q -> checkFormatFilterPath q











-- data Player = PlayerCtr {name :: String, age :: Int}
-- f PlayerCtr {name = n, age = a}
-- f PlayerCtr {name, age}
-- f PlayerCtr {..name}
-- f p = p.name
-- edit :: Player -> Player
-- edit player = player{name="Bart"}
-- edit player@PlayerCtr{..} = player{name = name ++ " suffixe"}
-- edit player@PlayerCtr{name = baseName} = player{name = baseName ++ " suffixe"}