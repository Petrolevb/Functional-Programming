module BlackJack where

import Wrapper
import Cards

{-
   Execution of size hand2

   size hand2
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
valueRank Jack 	      = 10
valueRank Queen       = 10
valueRank King        = 10
valueRank Ace         = 11

-- Return the value of a card
valueCard :: Card -> Integer
valueCard c = valueRank (rank c)

-- Return the number of aces in a hand
numberOfAces :: Hand->Integer
numberOfAces Empty 	  = 0
numberOfAces (Add (Card Ace _) h) = 1 + numberOfAces h
numberOfAces (Add _ h )   = numberOfAces h

-- Return the value of a hand
value :: Hand -> Integer
value Empty     = 0
value (Add c h) | numberOfAces (Add c h) > 1
                  = valueCard c + value h - (numberOfAces (Add c h) * 10)
                | numberOfAces (Add c h) == 1 && (valueCard c + value h) > 21
                  = valueCard c + value h - 10
                | otherwise = valueCard c + value h


-- From a given hand, determinate if loose
gameOver :: Hand -> Bool
gameOver n | (value n > 21) = True
gameOver n 		    = False


-- Determinate between two hands who wins Guest in first, Bank in second
winner :: Hand -> Hand -> Player
winner Empty Empty                                    = Bank
winner _ Empty                                        = Guest
winner h hb | (value h > value hb) && not(gameOver h) = Guest
winner h hb                                           = Bank


-- Add an Hand on the other
(<+) :: Hand -> Hand -> Hand
Empty <+ h      = h
h <+ Empty      = h
(Add c h) <+ hb =  (Add c (h <+ hb))



{-- Property for <+
prop_onTopOf assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf assoc p1 p2 p3 = p1 <+ (p2 <+ p3 ) == (p1 <+ p2 ) <+ p3
-}


-- Hand test
hand1 = Add (Card Ace Hearts) (Add (Card Ace Spades) Empty) -- 2
hand2 = Add (Card Ace Hearts) (Add (Card King Spades) Empty) -- 21
hand3 = Add (Card Ace Hearts) (Add (Card King Spades) (Add (Card (Numeric 8) Hearts) Empty)) -- 19
hand4 = Add (Card Ace Hearts) (Add (Card Ace Spades) (Add (Card (Numeric 8) Hearts) Empty)) -- 10
hand5 = Add (Card (Numeric 3) Hearts) (Add (Card King Spades) (Add (Card Queen Hearts) Empty)) -- 22