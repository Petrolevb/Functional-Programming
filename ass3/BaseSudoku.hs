module BaseSudoku where
{-
	Assignements Functionnal
		Sudoku
	BELLEC  | CHAUSSY
	Vincent | Erwan
-}

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
isSudoku s = undefined

-- isSolved check if it stay some empty cases
isSolved :: Sudoku -> Bool
isSolved s = and $ map (\x -> notElem Nothing x) (rows s)
