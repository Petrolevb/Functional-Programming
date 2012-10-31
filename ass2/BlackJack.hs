module BlackJack where

import Wrapper
import Cards


-- Define a function returning an empty hand

empty :: Hand = Empty


-- From a given hand, calculate the value
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Jack 	      = 10
valueRank Queen       = 10
valueRank King        = 10

valueCard :: Card -> Integer
valueCard c = valueRank (rank c)


numberOfAces :: Hand->Integer
numberOfAces Empty 	  = 0
numberOfAces (Add Aces h) = 1 + numberOfAces h
numberOfAces (Add _ h )   = numberOfAces h

value :: Hand -> Integer
value Empty     = 0
value (Add c h) = valueCard c + value h

-- From a given hand, determinate if loose
gameOver :: Hand -> Bool
gameOver n | (value n > 21) 	= True
gameOver n 			= False


-- Determinate between two hands who wins Guest in first, Bank in second
winner :: Hand -> Hand -> Player
winner Empty Empty 	           = Bank
winner _ Empty 			   = Guest
winner h hb | (value h > value hb) = Guest
winner h hb 			   = Bank


-- Add an Hand on the other
<+ :: Hand -> Hand -> Hand
Empty <+ h      = h
h <+ Empty      = h
(Add c h) <+ hb =  (Add c (h <+ hb))



-- Property for <+
prop_onTopOf assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf assoc p1 p2 p3 = p1 <+ (p2 <+ p3 ) == (p1 <+ p2 ) <+ p3
