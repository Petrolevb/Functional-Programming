module NineMen where

import Data.List (elem)

{-

Tableau de 24 cases 
win si 3 jetons alignés

1           2          3

     4      5     6
        
        7   8   9
        
10   11 12     13 14  15

        16 17  18

    19     20    21

22         23         24

Lignes 

123
456
789
101112
131415
161718
192021
222324

Colones
1 10 22
4 11 19
7 12 16
2 5  8
17 20 23
9 13 18
6 14 21
3 15 24

Diagonales
i + [0|1|2] * 3 (avec i = [1, 3, 16, 18])
147
369
161922
182124


-}

data Board = Board { line ::[(Case, Movement)] }
    deriving Show

data Token = Red | Black
    deriving (Show, Eq)

data Case = Token | Empty
    deriving (Show, Eq)

type Movement = [Int]

newBoard :: Board 
newBoard = Board [(Empty, (movementAllow x)) | x <- [1..24]]

-- The database is build such as the positions allowed are sorted
movementAllow :: Int -> Movement
movementAllow  1 = 2:10:[]
movementAllow  2 = 1:3:5:[]  
movementAllow  3 = 2:15:[]  
movementAllow  4 = 5:11:[]  
movementAllow  5 = 2:4:6:8:[]  
movementAllow  6 = 5:14:[]  
movementAllow  7 = 8:12:[]  
movementAllow  8 = 5:7:9:[]  
movementAllow  9 = 8:13:[]  
movementAllow 10 = 1:11:22:[]  
movementAllow 11 = 4:10:12:19:[]  
movementAllow 12 = 7:11:16:[]  
movementAllow 13 = 9:14:18:[]  
movementAllow 14 = 4:13:15:21:[]  
movementAllow 15 = 3:14:24:[]  
movementAllow 16 = 12:17:[]  
movementAllow 17 = 16:18:20:[]  
movementAllow 18 = 13:17:[]  
movementAllow 19 = 11:20:[]  
movementAllow 20 = 17:19:21:23:[]  
movementAllow 21 = 14:20:[]  
movementAllow 22 = 10:23:[]  
movementAllow 23 = 20:22:24:[]  
movementAllow 24 = 15:23:[]  


-- The board from a position to the second, return if allowed
-- Two reasons for disallow : the movement isn't correct or the case is already busy
isMovementAllowed :: Board -> Int -> Int -> Bool
isMovementAllowed b from to = let (_, pos) = line b !! from in
                                to `elem` pos && isCaseFree b to

isCaseFree :: Board -> Int -> Bool
isCaseFree b i = let (target, _) = line b !! i in target == Empty


