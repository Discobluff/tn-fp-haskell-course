-- Build me with: cabal build TP2.hs
-- Execute me with: cabal run -v0 TP2.hs
-- Load me in the REPL with: cabal repl TP2.hs, then use :r to reload the code upon changing

{- HLINT ignore -}

module Main where

import Debug.Trace
import GHC.Generics
import Generic.Random
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "TP2 is running"

-- If you don't like this TP, you can do TP3 instead.

-- Implement a card game a la Magic the Gathering. Each player
-- has 3 spots available to play cards like this:
--
--       Player 1
--
--  card1   card2   card3
--
--
--  carda   cardb   cardc
--
--       Player2
--
-- Each player starts with a deck of cards. When it is the turn of a
-- player, he can take cards from his hand and put them on the board.
-- After that, the turn resolves: each card of the player attacks the
-- card in front of it. If a player has no card in front of the attacking
-- card, then the attack contributes to the player's score. Each card
-- has two stats: its hitpoints and its attack. When a card attacks, it
-- deals the corresponding hitpoint to the opponent card.
-- Consider the diagram below:
--
--      Player 1
--
-- knight   empty   soldier
--
-- empty    soldier soldier
--
--      Player 2
--
-- with the knight having 2 hitpoints and 2 attacks, and the soldier having 1 hitpoint
-- and 1 attack. In this scenario, when player 1 attacks, the left knight
-- contributes two to the score while the right soldier kills its opponent (there
-- are only two cards: knight and soldier)
--
-- Exercise:
--
-- 1. Define a type for cards
--    - Define a Show instance for this type
-- 2. Define a type for the part of a player. Make it implement Show.
-- 3. Define a type for the whole board: the two players parts. Make it implement Show.
-- 4. Write a function making a player play
-- 5. Write a function playing an entire game
--    Display the list of boards while the game runs.
--
-- If you want to have randomness, use the 'random' function here:
-- https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html#v:random
-- Use mkStdGen to obtain a value that satisfies the constraint "RandomGen g":
-- https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html#t:StdGen
--
-- Use https://hoogle.haskell.org/ to find the functions you need

data Card = Knight Int Int | Soldier Int Int

defKnight :: Card
defKnight = Knight 2 2

defSoldier :: Card
defSoldier = Soldier 1 1

instance Show Card where
  show (Knight _ _ ) = "Knight"
  show (Soldier _ _) = "Soldier"

data Player = DefPlayer {
  deck :: [Card],
  score :: Int,
  card1 :: Maybe Card,
  card2 :: Maybe Card,
  card3 :: Maybe Card
}

instance Show Player where
  show (DefPlayer {card1, card2, card3}) = show card1 ++ "  " ++ show card2 ++ "  " ++ show card3

data Board = DefBoard Player Player

instance Show Board where
  show (DefBoard p1 p2) = show p1 ++ "\n" ++ show p2

-- instance Show (Maybe Card) where
--   show (Nothing) = "Empty"
--   show (Just card) = show card
-- showMaybeCard :: Maybe Card -> String
-- showMaybeCard Nothing = "Empty"
-- showMaybeCard (Just card) = show card

data Coup = DefCoup (Maybe Card) (Maybe Card) (Maybe Card)
data Turn = One | Two

play :: Board -> Turn -> Coup -> Board
play (DefBoard p1 p2) One coup = attack (DefBoard (placeCards p1 coup) p2) One
play (DefBoard p1 p2) Two coup = attack (DefBoard p1 (placeCards p2 coup)) Two

placeCards :: Player -> Coup -> Player
placeCards p (DefCoup c1 c2 c3) = p
  { card1 = place (card1 p) c1
  , card2 = place (card2 p) c2
  , card3 = place (card3 p) c3
  }

place :: Maybe Card -> Maybe Card -> Maybe Card
place Nothing newCard = newCard
place existing _ = existing

attack :: Board -> Turn -> Board
attack (DefBoard p1 p2) One =
  let p1' = p1
        { score = score p1 + laneScore (card1 p1) (card1 p2)
        , card1 = attackCards (card1 p1) (card1 p2)
        , card2 = attackCards (card2 p1) (card2 p2)
        , card3 = attackCards (card3 p1) (card3 p2)
        }
  in DefBoard p1' p2
attack (DefBoard p1 p2) Two =
  let p2' = p2
        { score = score p2 + laneScore (card1 p2) (card1 p1)
        , card1 = attackCards (card1 p2) (card1 p1)
        , card2 = attackCards (card2 p2) (card2 p1)
        , card3 = attackCards (card3 p2) (card3 p1)
        }
  in DefBoard p1 p2'

laneScore :: Maybe Card -> Maybe Card -> Int
laneScore (Just c) Nothing = attackValue c
laneScore _ _ = 0

attackValue :: Card -> Int
attackValue (Knight _ atk) = atk
attackValue (Soldier _ atk) = atk

hitpoints :: Card -> Int
hitpoints (Knight hp _) = hp
hitpoints (Soldier hp _) = hp

attackCards :: Maybe Card -> Maybe Card -> Maybe Card
attackCards Nothing c2 = c2
attackCards c1 Nothing = c1
attackCards (Just attacker) (Just defender)
  | hitpoints defender - attackValue attacker <= 0 = Nothing
  | otherwise = Just defender