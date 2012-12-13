module Board where

import Data.Maybe (isNothing, fromJust, fromMaybe)
import Data.Char
import GHC.IO
import System.Console.ANSI
import NineMen

instance Show Board where
         show = printBoard

printBoard :: Board -> String
printBoard b = printB' (line b) ""

printB' :: [(Case, a)] -> String -> String
printB' [] str = "\n"
printB' ((c, m):bs) str | -- Create the first and last line for the board
                     (21 < length list && length list <= 24)
                     || length list <= 3 = str ++ createLine ((c,m):take 2 bs)
                                               " --------------------- " "\n" ++
                                              printB' (drop 2 bs) str
                   | -- Create first and last line for the middle square
                     (18 < length list && length list <= 21)
                     || (3 < length list && length list <= 6)
                     = str ++ "|     "
                                   ++ createLine ((c,m):take 2 bs)
                                   " --------------- "
                                   "     |\n" ++
                          printB' (drop 2 bs) str
                   | -- Create the first and last line for the smallest square
                     (15 < length list && length list <= 18)
                     || (6 < length list && length list <= 9)
                     = str ++ "|     |     "
                                   ++ createLine ((c,m):take 2 bs)
                                      " --------- "
                                      "     |     |\n" ++
                          printB' (drop 2 bs) str
                   | -- Create the line in the middle of the board
                     otherwise = str ++ createLine ((c,m):take 2 bs)
                                             " --- "
                                             "                       " ++
                                   str ++ createLine (take 3 (drop 2 bs))
                                               " --- "
                                               "\n" ++
                                   printB' (drop 5 bs) str
  where list = (c, m):bs
        -- Create lines for output out of an array and some formated strings
        createLine (s:[]) dash after = getCase (fst s) ++ after
        createLine (s:ss) dash after = getCase (fst s) ++ dash ++ createLine ss dash after

getCase :: Case -> String
getCase c = case c of
                      Case NineMen.Red   -> "R"
                      Case NineMen.Black -> "B"
                      _                  -> "°"

gameLoop :: Board -> Token -> IO()
gameLoop b token | win       = showWin
           | lose      = showLose
           | otherwise = do 
              putStr $ "\ESC[2J" ++ show b
              print token
              putStrLn "Enter two numbers : token move, enter key then the position"
              moveFrom <- askPosition
              moveTo <- askPosition
              if Just token /= tokenAt b moveFrom
                 then gameLoop b token
                 else
                      let (tmpBoard, alignments) = createAlignment b moveFrom moveTo in
                      if isNothing tmpBoard 
                        then gameLoop b token
                        else if alignments
                               then do
                                  putStrLn "Enter a token to remove : "
                                  newBoard <- removeChoosenToken (fromJust tmpBoard)
                                  gameLoop newBoard (nextToken token)
                               else gameLoop (fromJust tmpBoard) (nextToken token)
    where 
        win                     = winner b == Just NineMen.Red
        lose                    = winner b == Just NineMen.Black
        showWin                 = putStrLn "Win Red"
        showLose                = putStrLn "Win Black"

nextToken :: Token -> Token
nextToken t = case t of
                   NineMen.Black -> NineMen.Red
                   _             -> NineMen.Black

removeChoosenToken :: Board -> IO Board
removeChoosenToken b = do
    pos <- askPosition
    let newBoard = removeToken b pos
    maybe (removeChoosenToken b) return newBoard

askPosition :: IO Int
askPosition = do
                putStr "Input :  "
                inp <- getLine
                case reads inp of
                  ((a,t1):_) | all isSpace t1 -> return a
                  _ -> do putStrLn "Incorrect input"
                          askPosition
