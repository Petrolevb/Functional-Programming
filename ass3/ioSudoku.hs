module IoSudoku where

import BaseSudoku
import System.IO
import Data.Char(digitToInt)

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