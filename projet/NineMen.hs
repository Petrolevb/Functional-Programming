module NineMen where

import Data.List (elem)
import Data.Maybe (isNothing, isJust, fromJust)

data Board = Board { line ::[(Case, Movement)] }

data Token = Red | Black
    deriving (Show, Eq)

data Case = Case Token | Empty
    deriving (Show, Eq)

type Movement = [Int]

newBoard :: Board 
newBoard = Board [(Empty, movementAllow x) | x <- [0..23]]

-- The database is build such as the positions allowed are sorted
dataMovements :: [Movement]
dataMovements = [[1,9], 
                 [0,2,4],
                 [1,14],
                 [4,10],
                 [1,3,5,7],
                 [4,13],
                 [7,11],
                 [4,6,8],
                 [7,12],
                 [0,10,21],
                 [3,09,11,18],
                 [6,10,15],
                 [8,13,17],
                 [3,12,14,20],
                 [2,13,23],
                 [11,16],
                 [15,17,19],
                 [12,16],
                 [10,19],
                 [16,18,20,22],
                 [13,19],
                 [9,22],
                 [19,21,23],
                 [14,22] ]
dataAlignements :: [([Int], [Int])]
dataAlignements = [([0,1,2], [0,9,21]),
                   ([0,1,2], [1,4,7]),
                   ([0,1,2], [2,14,23]),
                   ([3,4,5], [3,10,18]),
                   ([3,4,5], [1,4,7]),
                   ([3,4,5], [5,13,20]),
                   ([6,7,8], [6,11,15]),
                   ([6,7,8], [1,4,7]),
                   ([6,7,8], [8,12,17]),
                   ([9,10,11], [0,9,21]),
                   ([9,10,11], [3,10,18]),
                   ([9,10,11], [6,11,15]),
                   ([12,13,14], [8,12,17]),
                   ([12,13,14], [0,13,20]),
                   ([12,13,14], [2,14,23]),
                   ([15,16,17], [6,11,15]),
                   ([15,16,17], [16,19,22]),
                   ([15,16,17], [8,12,17]),
                   ([18,19,20], [3,10,18]),
                   ([18,19,20], [16,19,22]),
                   ([18,19,20], [0,19,21]),
                   ([21,22,23], [0,9,21]),
                   ([21,22,23], [16,19,22]),
                   ([21,22,23], [2,14,23])]

movementAllow :: Int -> Movement
movementAllow i = dataMovements !! i 


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


createAlignment :: Board -> Int -> Int -> (Maybe Board, Bool)
createAlignment b i1 i2 = let newBoard = deplacement b i1 i2 in
                                if isNothing newBoard then (Nothing, False)
                                else (newBoard, isJust $ isAlign (fromJust newBoard) i2)


-- Return if the board is finish: 
--      one player have less than three token or
--      a player can not move
isFinish :: Board -> Bool
isFinish b = (numberToken b Red < 3) || (numberToken b Black < 3)   ||
            -- if there is no token able to move
             not (any (canMove b) (positionToken b Red))     ||
             not (any (canMove b) (positionToken b Black))

winner :: Board -> Maybe Token
winner b | not $ isFinish b                                                       = Nothing
         | (numberToken b Red < 3) || not (any (canMove b) (positionToken b Red)) = Just Black
         | otherwise                                                              = Just Red

numberToken :: Board -> Token -> Int
numberToken b t = length $ filter (\(c, _) -> c == Case t) (line b)

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
positionToken b t = [x | (x, Case t) <- zip [0..] (map fst (line b))]

-- Return the token at the position given
tokenAt :: Board -> Int ->  Maybe Token
tokenAt b i = let (t, _) = line b !! i in
              if t == Empty then Nothing
              else 
                let (Case return) = t in 
                Just return


isAlign :: Board -> Int -> Maybe Token
isAlign b i = testCases $ map (getTuplet b) ((\(fst, snd) -> [fst, snd]) $ dataAlignements !! i)

testCases :: [[Case]] -> Maybe Token
testCases []     = Nothing
testCases (c:cs) = if Empty `elem` c then testCases cs
                   else
                    let (Case tok:cother) = c in
                        -- If the three token are equal 
                        if   areEqual tok cother then Just tok 
                        else testCases cs
                where 
                areEqual _ [] = True
                areEqual Red   (Case Red:cs)   = areEqual Red cs
                areEqual Black (Case Black:cs) = areEqual Black cs
                areEqual _     _          = False

-- Get tokens at these places
getTuplet :: Board -> [Int] -> [Case]
getTuplet b is = [ fst $ snd x | x <- zip [0..] (line b), fst x `elem` is]
{- 
  - I finally found how to make a list comprehension here, but it looks a bit weird, 
  - thus I'm able to every suggestions that you can advice
-}

removeToken :: Board -> Int -> Maybe Board
removeToken b i | isJust (isAlign b i) = Nothing
                | otherwise            = Just (changeCase b i Empty)

test = changeCase (changeCase (changeCase (changeCase newBoard 0 (Case Red)) 1 (Case Black)) 9 (Case Red)) 2 (Case Red)
test2= changeCase (changeCase test 3 (Case Black)) 4 (Case Black)
test3 = changeCase test2 22 (Case Red)
test4 = changeCase test3 6 (Case Black)
test5 = changeCase (changeCase (changeCase (changeCase (changeCase (changeCase (changeCase newBoard 0 (Case Red)) 1 (Case Black)) 9 (Case Black)) 2 (Case Red)) 14 (Case Black)) 21 (Case Black)) 23 (Case Red)
