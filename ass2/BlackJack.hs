module BlackJack where

import Wrapper
import Cards
import Test.QuickCheck
import Data.List

{-
   Execution of size hand

   size hand
   = size (Add (Card (Numeric 2) Hearts)
               (Add (Card Jack Spades) Empty))
   = 1 + size (Add (Card Jack Spades) Empty)
   = 1 + 1 + size (Empty)
   = 1 + 1 + 0
   = 2
-}

-- Define a function returning an empty hand

empty :: Hand
empty = Empty


-- From a given hand, calculate the value
-- Return the rank of a card
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace         = 1
valueRank _           = 10

-- Return the value of a card
valueCard :: Card -> Integer
valueCard c = valueRank (rank c)

-- Return the number of aces in a hand
numberOfAces :: Hand->Integer
numberOfAces Empty                = 0
numberOfAces (Add (Card Ace _) h) = 1 + numberOfAces h
numberOfAces (Add _ h )           = numberOfAces h

-- Return the value of a hand when there is more than one ace
valueMultiAces :: Hand -> Integer
valueMultiAces Empty     = 0
valueMultiAces (Add c h) = valueMultiAces h + valueCard c 

-- Return the value of a hand
value :: Hand -> Integer
value Empty                                   = 0
value (Add c h)	| numberOfAces (Add c h) > 1  = valueMultiAces (Add c h)
		| numberOfAces (Add c h) == 1 = oneAce
                | otherwise                   = valueCard c + value h
	where oneAce =  if (valueCard c + value h + 10) > 21
			then valueMultiAces (Add c h)
			else valueCard c + value h + 10


-- From a given hand, determinate if loose
gameOver :: Hand -> Bool
gameOver n = value n > 21


-- Determinate between two hands who wins Guest in first, Bank in second
winner :: Hand -> Hand -> Player
winner h hb | gameOver h  = Bank
            | gameOver hb = Guest
            | otherwise   = if value h > value hb then Guest else Bank


-- Add an Hand on the other
infixr 5 <+ -- same priority as :
(<+) :: Hand -> Hand -> Hand
Empty <+ h      = h
h <+ Empty      = h
(Add c h) <+ hb =  Add c (h <+ hb)


-- Property for <+
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3 ) == (p1 <+ p2 ) <+ p3

prop_onTopOf :: Hand -> Hand -> Bool
prop_onTopOf p1 p2 = size(p1 <+ p2) == size p1 + size p2


-- Creating a full Deck

createSuit :: Suit -> Hand -> [Rank] -> Hand
createSuit s hand [] = hand
createSuit s hand (rank:ranks) = Add (Card rank s) (createSuit s hand ranks)

addSuits :: [Suit] -> Hand
addSuits [] = Empty
addSuits (lSuit:ls) = createSuit lSuit Empty lRank <+ (addSuits ls)
  where lRank = ([Numeric x | x <- [2..10]] ++ [Jack, Queen, King, Ace])

fullDeck :: Hand
fullDeck = addSuits lSuit
  where 
        lSuit = [Spades, Hearts, Clubs, Diamonds]



-- Hand test
hand1 = Add (Card Ace Hearts) (Add (Card Ace Spades) Empty) -- 2
hand2 = Add (Card Ace Hearts) (Add (Card King Spades) Empty) -- 21
hand3 = Add (Card Ace Hearts) (Add (Card King Spades) (Add (Card (Numeric 8) Hearts) Empty)) -- 19
hand4 = Add (Card Ace Hearts) (Add (Card Ace Spades) (Add (Card (Numeric 8) Hearts) Empty)) -- 10
hand5 = Add (Card (Numeric 3) Hearts) (Add (Card King Spades) (Add (Card Queen Hearts) Empty)) -- 23
hand6 = Add (Card Ace Spades) (Add (Card Ace Diamonds) (Add (Card King Spades) (Add (Card Ace Hearts) Empty))) -- 13
hand7 = Add (Card (Numeric 8) Hearts) (Add (Card Ace Diamonds)  (Add (Card (Numeric 3) Hearts) Empty)) -- 12
