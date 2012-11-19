module BaseSudoku where
{-
	Assignements Functionnal
		Sudoku
	BELLEC  | CHAUSSY
	Vincent | Erwan
-}
import Test.QuickCheck

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
             isTrue isLength9 s &&
             isTrue isValidNumbers s
  where isLength9      s = length s == 9
        isValidNumbers   = all isBounded
        isBounded      a = a `elem` (Nothing : map Just [1..9])

isTrue :: ([Maybe Int] -> Bool) -> Sudoku -> Bool
isTrue p s | length (rows s) == 1 = p (head $ rows s)
isTrue p s = p (head $ rows s) && isTrue p (Sudoku $ tail $ rows s)


-- isSolved check if it stay some empty cases
isSolved :: Sudoku -> Bool
isSolved s = all (notElem Nothing) (rows s)


-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = undefined


-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)



example :: Sudoku
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