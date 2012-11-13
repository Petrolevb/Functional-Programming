
import BaseSudoku


-- Function to print the Sudoku
printSudoku :: Sudoku -> IO ()
printSudoku s = do
  printLine $ map (\x -> map (\y -> printCase y) x) (rows s)

-- Cette fonction affiche toutes les lignes d'un tableau de string
printLine :: [String] -> IO ()
printLine [] = return ()
printLine (a:as) = do 
  putStrLn a
  printLine as


-- This function will convert the Maybe Int into a single char
printCase ::(Maybe Int) -> Char
printCase Nothing = '.'
printCase (Just n) = show n !! 0
