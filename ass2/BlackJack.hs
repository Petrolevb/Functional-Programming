module BlackJack where

import Wrapper
import Cards
import Test.QuickCheck
import System.Random

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
value Empty          = 0
value h | val' <= 21 = val'
        | otherwise  = val
  where val  = valueMultiAces h
        val' = val + 10 * numberOfAces h


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
-- Create the 13 cards of a Suit
createSuit :: Suit -> [Rank] -> Hand
createSuit _ []           = Empty
createSuit s (rank:ranks) = Add (Card rank s) (createSuit s ranks)

-- Add all cards from createSuit together
addSuits :: [Suit] -> Hand
addSuits []         = Empty
addSuits (lSuit:ls) = createSuit lSuit lRank <+ addSuits ls
  where lRank = [Numeric x | x <- [2..10]] ++ [Jack, Queen, King, Ace]

-- Create a full deck of cards
fullDeck :: Hand
fullDeck = addSuits lSuit
  where lSuit = [Spades, Hearts, Clubs, Diamonds]


-- Draw the first card of the deck to put it in the hand
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ = error "draw: The deck is empty"
draw (Add d deck) hand = (deck, Add d hand)

-- Play for the bank until it has 16 or more
playBank :: Hand -> Hand
playBank deck = playBank' (deck, Empty)

playBank' :: (Hand, Hand) -> Hand
playBank' (deck, bankHand) | value bankHand < 16 = playBank' (deck', bankHand')
                           | otherwise           = bankHand
  where (deck', bankHand') = draw deck bankHand


-- The Idea is to take a random card, remove it from the deck and add it to a new deck
shuffle :: StdGen -> Hand -> Hand
shuffle _ Empty = Empty
shuffle g h = 
--	(x, y) = randomR(1, size h)
--	c = takeCardNum x h
	Add c (shuffle y (removeCard c h))
	where 
		(x, y) = randomR(1, size h) g
		c = takeCardNum x h
	
	-- select an arbitrary card
	-- remove from the new card
	-- return the shuffled hand wich stay


removeCard :: Card -> Hand -> Hand
removeCard _ Empty = Empty
removeCard card (Add c h) | card == c = h
			  | otherwise = Add c (removeCard card h)

belongsTo :: Card -> Hand -> Bool
belongsTo _ Empty      = False
belongsTo c (Add c' h) = c == c'|| c `belongsTo` h


takeCardNum :: Integer -> Hand -> Card
takeCardNum 1 (Add c h) = c
takeCardNum n (Add c h) = takeCardNum (n-1) h



prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffle g h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffle g h)


-- Hand test
hand1 = Add (Card Ace Hearts) (Add (Card Ace Spades) Empty) -- 2
hand2 = Add (Card Ace Hearts) (Add (Card King Spades) Empty) -- 21
hand3 = Add (Card Ace Hearts) (Add (Card King Spades) (Add (Card (Numeric 8) Hearts) Empty)) -- 19
hand4 = Add (Card Ace Hearts) (Add (Card Ace Spades) (Add (Card (Numeric 8) Hearts) Empty)) -- 10
hand5 = Add (Card (Numeric 3) Hearts) (Add (Card King Spades) (Add (Card Queen Hearts) Empty)) -- 23
hand6 = Add (Card Ace Spades) (Add (Card Ace Diamonds) (Add (Card King Spades) (Add (Card Ace Hearts) Empty))) -- 13
hand7 = Add (Card (Numeric 8) Hearts) (Add (Card Ace Diamonds)  (Add (Card (Numeric 3) Hearts) Empty)) -- 12
