module BaseSudoku where
{-
	Assignements Functionnal
		Sudoku
	BELLEC  | CHAUSSY
	Vincent | Erwan
-}
import Test.QuickCheck
import Data.List(transpose)

data Sudoku = Sudoku { rows:: [[Maybe Int]] }
	deriving (Show)

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
  where isLength9      s = length s == 9
        isValidNumbers   = all isBounded
        isBounded      a = a `elem` (Nothing : map Just [1..9])

-- High order function for computing informations on rows
computeOnRows :: ([Maybe Int] -> Bool) -> Sudoku -> Bool
computeOnRows p s | length (rows s) == 1 = p (head $ rows s)
computeOnRows p s = p (head $ rows s) && isTrue p (Sudoku $ tail $ rows s)


-- isSolved check if it stay some empty cases
isSolved :: Sudoku -> Bool
isSolved s = all (notElem Nothing) (rows s)


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



type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock []           = True
isOkayBlock (Nothing:bs) = isOkayBlock bs
isOkayBlock (b:bs)       = b `notElem` bs && isOkayBlock bs


blocks :: Sudoku -> [Block]
blocks s = rows s ++ transpose (rows s) ++ buildBlocks s

buildBlocks :: Sudoku -> [Block]
buildBlocks s = 
	concatMap (take 3) (take 3 $ rows s) : 
	concatMap (take 3) (map (drop 3) (take 3 $ rows s)) :
	concatMap (drop 6) (take 3 $ rows s) :
	concatMap (take 3) (take 3 $ drop 3 $ rows s) :
	concatMap (take 3) (map (drop 3) (take 3 $ drop 3 $ rows s)) :
	concatMap (drop 6) (take 3 $ drop 3 $ rows s) :
	concatMap (take 3) (drop 6 $ rows s) :
	concatMap (take 3) (map (drop 3) (drop 6 $ rows s)) :
	[concatMap (drop 6) (drop 6 $ rows s)]



isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)



-- Property which test that for a given Sudoku, there are 3*9 blocks and each of them
-- has a length of 9 cells
prop_IsBlocksSudokuOK :: Sudoku -> Bool
prop_IsBlocksSudokuOK s = (length y == 3*9) && all (\x -> length x == 9) y
	where 
		y = blocks s


example = Sudoku
  [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
  , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
  , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
  , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
  , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
  , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
  , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
  , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
  , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
  ]
