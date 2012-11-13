{-
	Assignements Functionnal
		Sudoku
	BELLEC  | CHAUSSY
	Vincent | Erwan
-}

data Sudoku = Sudoku { rows:: [[Maybe Int]] }

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
isSudoku s = False

isRow :: [Maybe Int] -> Int -> Bool
isRow (a:[]) 1 = True
isRow (a:as) 0        = False
isRow [] _          = False
isRow(a:as) n        = isRow as (n-1)
