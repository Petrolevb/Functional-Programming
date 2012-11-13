
import BaseSudoku


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
printCase (Just n) = head (show n)
