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

data Case = Case Token | Empty
    deriving (Show, Eq)

type Movement = [Int]

newBoard :: Board 
newBoard = Board [(Empty, movementAllow x) | x <- [0..23]]

-- The database is build such as the positions allowed are sorted
movementAllow :: Int -> Movement
movementAllow  0 = [1, 9]
movementAllow  1 = [0,2,4]  
movementAllow  2 = [1,14]  
movementAllow  3 = [4,10]
movementAllow  4 = [1,3,5,7]
movementAllow  5 = [4,13]
movementAllow  6 = [7,11]
movementAllow  7 = [4,6,8]
movementAllow  8 = [7,12]
movementAllow  9 = [0,10,21]
movementAllow 10 = [3,09,11,18]
movementAllow 11 = [6,10,15]
movementAllow 12 = [8,13,17]
movementAllow 13 = [3,12,14,20]
movementAllow 14 = [2,13,23]
movementAllow 15 = [11,16]
movementAllow 16 = [15,17,19]
movementAllow 17 = [12,16]
movementAllow 18 = [10,19]
movementAllow 19 = [16,18,20,22]
movementAllow 20 = [13,19]
movementAllow 21 = [9,22]
movementAllow 22 = [19,21,23]
movementAllow 23 = [14,22]


-- The board from a position to the second, return if allowed
-- Two reasons for disallow : the movement isn't correct or the case is already busy
isMovementAllowed :: Board -> Int -> Int -> Bool
isMovementAllowed b from to = let (token, pos) = line b !! from in
                                to `elem` pos && isCaseFree b to && Empty /= token

isCaseFree :: Board -> Int -> Bool
isCaseFree b i = let (target, _) = line b !! i in target == Empty


-- Transform the case targeted (for exemple, replace a red token by an empty case)
changeCase :: Board -> Int -> Case -> Board
changeCase b i c = Board (take i (line b) ++ [(c, movementAllow i)] ++ drop (i+1) (line b))

deplacement :: Board -> Int -> Int -> Maybe Board
deplacement b i1 i2 | not $ isMovementAllowed b i1 i2 = Nothing
                    | otherwise                       = 
                        let (tok, _) = line b !! i1 in
                        Just $ changeCase (changeCase b i2 tok) i1 Empty


-- Return if the board is finish: 
--      one player have less than three token or
--      a player can not move
isFinish :: Board -> Bool
isFinish b = (numberToken b Red < 3) || (numberToken b Black < 3)   ||
            -- if there is no token able to move
             (not $ or $ map (canMove b) (positionToken b Red))     ||
             (not $ or $ map (canMove b) (positionToken b Black))

winner :: Board -> Maybe Token
winner b | not $ isFinish b                                                              = Nothing
         | (numberToken b Red < 3) || (not $ or $ map (canMove b) (positionToken b Red)) = Just Black
         | otherwise                                                                     = Just Red

numberToken :: Board -> Token -> Int
numberToken b t = count (line b) t
    where 
        count [] _                         = 0
        count ((c, _):s) t | Case t == c = 1 + count s t
                           | otherwise     = count s t

-- Say If the position targeted can move
canMove :: Board -> Int -> Bool
canMove b i = not $ null (movements b i)

-- Given a position, return the possible movements
movements :: Board -> Int -> Movement
movements b i = let (tok, pos) = line b !! i in
                 if tok == Empty then [] else
                 concatMap (\(bool, to) -> [to | bool]) (zip (map (isMovementAllowed b i) pos) pos)

-- Return all posistions for a given token
positionToken :: Board -> Token -> [Int]
positionToken b t = addPos 0 (line b) t
        where addPos _ []         _ = []
              addPos i ((c, _):s) t | Case t == c = i : addPos (i+1) s t
                                    | otherwise   = addPos (i+1) s t

test = changeCase (changeCase (changeCase (changeCase newBoard 0 (Case Red)) 1 (Case Black)) 9 (Case Red)) 2 (Case Red)
test2= changeCase (changeCase test 3 (Case Black)) 4 (Case Black)
