module Sudoku where

{-
	Assignements Functionnal
		Sudoku
	BELLEC  | CHAUSSY
	Vincent | Erwan
-}

import Test.QuickCheck
import System.IO
import Data.Char(digitToInt)
import Data.List(transpose, isInfixOf)
import Data.Maybe(isNothing, fromJust)

data Sudoku = Sudoku { rows:: [[Maybe Int]] }
	deriving (Show)


-- Part A

-- Return an empty Sudoku
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [ y | x <-[1..9] ]
	where y = [ Nothing | z <-[1..9] ]
{-
This function returns 9 ys, 
where y is 9 empty arrays
-}


-- A sudoku is a 9 row by 9 column cases full of digit
isSudoku :: Sudoku -> Bool
isSudoku s = length (rows s) == 9 &&
             computeOnRows isLength9 s &&
             computeOnRows isValidNumbers s
  where 
        -- check if lines are of length 9
        isLength9      s = length s == 9
        -- Check that numbers in the Sudoku are between 1 and 9 or Nothing
        isValidNumbers   = all isBounded
        isBounded      a = a `elem` (Nothing : map Just [1..9])

-- High order function for computing informations on rows
computeOnRows :: ([Maybe Int] -> Bool) -> Sudoku -> Bool
computeOnRows p s | length (rows s) == 1 = p (head $ rows s)
computeOnRows p s = p (head $ rows s) &&
                    computeOnRows p (Sudoku $ tail $ rows s)


-- isSolved check if it stay some empty cases
isSolved :: Sudoku -> Bool
isSolved s = all (notElem Nothing) (rows s)



-- Part B

-- Function to print the Sudoku
printSudoku :: Sudoku -> IO ()
printSudoku s = printLine $ map (map printCase) (rows s)

-- Cette fonction affiche toutes les lignes d'un tableau de string
printLine :: [String] -> IO ()
printLine [] = return ()
printLine (a:as) = do 
  putStrLn a
  printLine as

-- This function will convert the Maybe Int into a single char
printCase :: Maybe Int -> Char
printCase Nothing = '.'
printCase (Just n) = head $ show n


-- Read a Sudoku from a file
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
	s <- readFile fp
        let x = createSudoku $ lines s
        return $ if isSudoku x then x else error "Not a Sudoku"

-- Parse the string into a Sudoku
createSudoku :: [String] -> Sudoku
createSudoku = Sudoku . map createLine 
-- map createLine is the equivalent for some functions
-- which could be called "createBoard"
  where 
        createLine []       = []
	createLine ('.':as) = Nothing : createLine as
        createLine (a  :as) = (Just $ digitToInt a) : createLine as



-- Part C

-- cell generates an arbitrary cell in a Sudoku
-- 	Probability : 90% empty, 10% between 1-9
cell :: Gen (Maybe Int)
cell = frequency 
 [(90, return Nothing),
  (10, do n <- choose(1,9)
          return (Just n))]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)


-- Check whether a Sudoku is valid or not
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku



-- Part D

type Block = [Maybe Int]

-- Check if the block is correct : it does not contain the same number twice
isOkayBlock :: Block -> Bool
isOkayBlock []           = True
isOkayBlock (Nothing:bs) = isOkayBlock bs
isOkayBlock (b:bs)       = b `notElem` bs && isOkayBlock bs


-- Construct a list of all blocks of a Sudoku
blocks :: Sudoku -> [Block]
blocks s = rows s ++ transpose (rows s) ++ buildBlocks s

-- Create the list of block of size 3*3
buildBlocks :: Sudoku -> [Block]
buildBlocks s = cutBlocks (take 3 (rows s)) ++
                 cutBlocks (take 3 (drop 3 (rows s))) ++
                 cutBlocks (drop 6 (rows s))

-- Cut 3 lines as 3*3 block
cutBlocks s = [get s, get (leave 3 s), get (leave 6 s)]
  where get   = concatMap (take 3)
        leave x = map (drop x) 


-- Property which test that for a given Sudoku
-- there are 3*9 blocks
-- and each of them has a length of 9 cells
prop_IsBlocksSudokuOK :: Sudoku -> Bool
prop_IsBlocksSudokuOK s = (length y == 3*9) && all (\x -> length x == 9) y
  where y = blocks s


-- Verifies if all blocks in a Sudoku are valid 
isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)



-- Part E

type Pos = (Int,Int)


-- Return a list of blank cell
blanks :: Sudoku -> [Pos]
blanks s = concatMap inRow ([0..8] `zip` map extractBlank (rows s))
  where
        -- Extract the index of each Nothing in each row
        extractBlank s = map snd (filter (isNothing . fst) (s `zip` [0..8]))
        -- Create pair of row number with column number for blank spot
        inRow (x,[])   = []
        inRow (x,y:ys) = (x, y) : inRow (x,ys)

-- Property that states that all cells in the blanks list are actually blank.
prop_Blanks :: Sudoku -> Bool
prop_Blanks s = and $ mapBlanks (rows s) (blanks s)
  where mapBlanks s = map (containNothing s)
        containNothing s (l,r) = isNothing ((s !! l) !! r)


-- Replace a value in a list
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) s (i,v)  | i < 0 || i >= length s = s
(!!=) s (i, v) = take i s ++  [v] ++ drop (i+1) s

-- Property that states the length of the list is not changed
prop_replace :: [Maybe Int] -> (Int, Maybe Int) -> Bool
prop_replace s (i,v) = length s == length (s !!= (i,v))


-- Update a cell in a Sudoku
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update s (row, column) v | correct row column =
                         Sudoku (
                                take row (rows s) ++
                                newLine ++
                                drop (row+1) (rows s))
                         | otherwise = s
  where 
        -- Check that row and column are correct
        correct row column = 0 <= row && row < 9 && 0 <= column && column < 9
        -- Constructs the new line
        newLine = [rows s !! row !!= (column, v)]


-- Property that states the cell has been updated
-- Generate positions
gPos :: Gen Pos
gPos = do 
            x <- choose(0,8)
            y <- choose(0,8)
            return (x,y)

-- Porperty to use with QuickCheck
prop_Update :: Sudoku -> Pos -> Maybe Int -> Property
prop_Update s pos v = forAll gPos $
                  \p -> prop_Up s p v


prop_Up :: Sudoku -> Pos -> Maybe Int -> Bool
prop_Up s (r, c) v = rows (update s (r,c) v) !! r !! c == v


-- Create a list of all possible solutions for a position
candidates :: Sudoku -> Pos -> [Int]
candidates s (r,c) = map fromJust [x | x <- map Just [1..9],
                                   x `notElem` line &&
                                   x `notElem` column &&
                                   x `notElem` block]
  where x = blocks s
        line = x !! r
        column = x !! (9+c)
        block = x !! (18 + (c `div`3) + (3 * (r `div` 3)))


-- Property that try if the possible values produce a valid Sudoku
prop_Possible_value s = forAll gPos $
                        \p -> prop_Candidate s p

prop_Candidate :: Sudoku -> Pos -> Property
prop_Candidate s pos = isOkay s ==>
                 all (up s pos) values
  where values = map Just (candidates s pos)
        up s pos value = isOkay (tmpSudoku value)
        tmpSudoku = update s pos


-- Solve a given Sudoku
solve :: Sudoku -> Maybe Sudoku
solve s | not(isSudoku s && isOkay s) = Nothing
solve s = solve' s

solve' :: Sudoku -> Maybe Sudoku
solve' s = tryPositions s (blanks s)

-- Try the first blank position
tryPositions :: Sudoku -> [Pos] -> Maybe Sudoku
tryPositions s [] = Just s
tryPositions s (p:ps) = tryValues s p values
  where values = candidates s p

-- Try the first value for a geven position
tryValues :: Sudoku -> Pos -> [Int] -> Maybe Sudoku
tryValues s p [] = Nothing
tryValues s p (v:vs) = case solve' updateSud of
                            Nothing  -> tryValues s p vs
                            Just sol -> Just sol
  where updateSud = update s p (Just v)


-- read and solve a sudoku from a file
readAndSolve :: FilePath -> IO ()
readAndSolve path = do
                     sud <- readSudoku path
                     let result = solve sud
                     printSudoku $ fromJust result


-- State if a sudoku is a solution of another sudoku
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s1 s2 | isOkay s1 && null (blanks s1)
                     = and $ inRows (rows s1) (rows s2)
                   | otherwise = False
  where inRows [] []              = [True]
        inRows (s1:s1s) (s2:s2s)  = (and $ inLines s1 s2) : inRows s1s s2s
        inLines [] []             = [True]
        inLines (s1:s1s) (s2:s2s) = (isNothing s2 || s1 == s2)
                                    : inLines s1s s2s


-- Property that states the soundness of the function solve
prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = isOkay s ==>
                    fromJust (solve s) `isSolutionOf` s



ex = Sudoku
   [ [Just 3, Just 6, Nothing, Nothing, Just 7, Just 1, Just 2, Nothing, Nothing]
  , [Nothing, Just 5, Nothing, Nothing, Nothing, Nothing ,Just 1, Just 8, Nothing]
  , [Nothing, Nothing, Just 9, Just 2, Nothing, Just 4, Just 7, Nothing, Nothing]
  , [Nothing, Nothing, Nothing, Nothing, Just 1, Just 3, Nothing, Just 2, Just 8]
  , [Just 4, Nothing, Nothing,Just 5, Nothing, Just 2, Nothing, Nothing, Just 9]
  , [Just 2, Just 7, Nothing, Just 4, Just 6, Nothing, Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Just 5, Just 3, Nothing, Just 8, Just 9, Nothing, Nothing]
  , [Nothing, Just 8, Just 3, Nothing, Nothing, Nothing, Nothing, Just 6, Nothing]
  , [Nothing, Nothing, Just 7, Just 6, Just 9, Nothing, Nothing, Just 4, Just 3]
  ]

solved = Sudoku
  [ [Just 3, Just 6, Just 4, Just 8, Just 7, Just 1, Just 2, Just 9, Just 5]
  , [Just 7, Just 5, Just 2, Just 9, Just 3, Just 6 ,Just 1, Just 8, Just 4]
  , [Just 8, Just 1, Just 9, Just 2, Just 5, Just 4, Just 7, Just 3, Just 6]
  , [Just 5, Just 9, Just 6, Just 7, Just 1, Just 3, Just 4, Just 2, Just 8]
  , [Just 4, Just 3, Just 1, Just 5, Just 8, Just 2, Just 6, Just 7, Just 9]
  , [Just 2, Just 7, Just 8, Just 4, Just 6, Just 9, Just 3, Just 5, Just 1]
  , [Just 6, Just 4, Just 5, Just 3, Just 2, Just 8, Just 9, Just 1, Just 7]
  , [Just 9, Just 8, Just 3, Just 1, Just 4, Just 7, Just 5, Just 6, Just 2]
  , [Just 1, Just 2, Just 7, Just 6, Just 9, Just 5, Just 8, Just 4, Just 3]
  ]

test = Sudoku
  [ [Just 3, Nothing, Just 4, Just 8, Nothing, Just 1, Just 2, Just 9, Just 5]
  , [Just 7, Just 5, Just 2, Just 9, Just 3, Just 6 ,Just 1, Just 8, Just 4]
  , [Just 8, Just 1, Just 9, Just 2, Just 5, Just 4, Just 7, Just 3, Just 6]
  , [Just 5, Just 9, Nothing, Just 7, Just 1, Just 3, Just 4, Just 2, Just 8]
  , [Just 4, Just 3, Just 1, Just 5, Just 8, Just 2, Just 6, Just 7, Just 9]
  , [Just 2, Nothing, Just 8, Nothing, Nothing, Just 9, Just 3, Just 5, Just 1]
  , [Just 6, Just 4, Just 5, Just 3, Just 2, Just 8, Just 9, Just 1, Just 7]
  , [Just 9, Just 8, Just 3, Just 1, Just 4, Just 7, Nothing, Just 6, Just 2]
  , [Just 1, Just 2, Just 7, Just 6, Just 9, Just 5, Just 8, Just 4, Just 3]
  ]

--------------------------
{-
   We have a problem when we try to use solve'' with allBlankSudoku.
   Apparently the program is stuck in an infinite loop
   and we do not know why at this moment.
   Then from small test i have done, we can only resolve
   "easy1.sud" and the example
-}
{-
solve :: Sudoku -> Maybe Sudoku
solve s = test s 50

test :: Sudoku -> Int -> Maybe Sudoku
test s i | i == 0 = Just s
         | otherwise = test (fromJust $ solve'' s) (i-1)

-- Original solve function
solve'' :: Sudoku -> Maybe Sudoku
solve'' s | not(isSudoku s && isOkay s) = Nothing
solve'' s = solve' s

solve' :: Sudoku -> Maybe Sudoku
solve' sud | null $ blanks sud = Just sud
solve' sud = if isNothing (resolve sud) then tryValue else resolve sud
  where tryValue = fixValue tmpSud listPosValue
        tmpSud = updatePositions sud (getEasyCandidate sud)
        listPosValue = concatMap (getCandidate sud) [2..9]

-- Fix a value and try to solve the new sudoku
fixValue :: Sudoku -> [(Pos, [Int])] -> Maybe Sudoku
fixValue sud [] = Just sud
fixValue sud ((p, a:as):ss) = Just (update sud p (Just a))
{-
   In the last line, when we replace Just by solve''
   we have a problem for solving allBlankSudoku in an infinite loop.
   We do not understand why at this moment
-}

-- Solve the sudoku when there is obvious results for cells
resolve :: Sudoku -> Maybe Sudoku
resolve s = if  null (getEasyCandidate s) 
	    then Nothing 
	    else solve $ updatePositions s (getEasyCandidate s)

-- Update the value of different positions
updatePositions :: Sudoku -> [(Pos, Int)] -> Sudoku
updatePositions s [] = s
updatePositions s ((p, n):ls) = updatePositions (update s p (Just n)) ls

-- get a list of pairs of position with 'size' corresponding candidate
getCandidate :: Sudoku -> Int -> [(Pos, [Int])]
getCandidate s size = removeNotEasy (map (\(a,b) -> (a, length b, b))
                                   (getPosCandidate s))
  where removeNotEasy [] = []
        removeNotEasy ( (p , n, a) : ss ) | n == size
                                               = (p, a) : removeNotEasy ss
	           			     | otherwise = removeNotEasy ss
	getPosCandidate s = zip (blanks s) (map (candidates s) (blanks s))

-- get a list of pairs of position with only one corresponding candidate
getEasyCandidate :: Sudoku -> [(Pos, Int)]
getEasyCandidate s = clear $ getCandidate s 1
  where 
        clear [] = []
        clear ((pos, a:as):ss) = (pos, a) : clear ss
-}