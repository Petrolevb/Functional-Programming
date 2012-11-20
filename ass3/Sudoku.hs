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
import Data.List(transpose)

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

