module BlackJack where

import Wrapper
import Cards


-- Define a function returning an empty hand

empty :: Hand = []


-- From a given hand, calculate the value
valueRank :: Rank -> Integer
valueRank 

valueCard :: Card -> Integer

value :: Hand -> Integer


-- From a given hand, determinate if loose
gameOver :: Hand -> Bool
gameOver n | (value n > 21) = True
gameOver n = False


-- Determinate between two hands who wins
winner :: Hand -> Hand -> Player
