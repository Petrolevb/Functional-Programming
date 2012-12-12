module Board where

import Data.Maybe (isNothing, fromJust)
import Data.Char
import GHC.IO
import System.Console.ANSI
import NineMen

printBoard :: Board -> IO()
printBoard b = do putStr "\ESC[2J"
                  printB $ line b

printB :: [(Case, a)] -> IO()
printB [] = putStrLn ""
printB ((c, m):bs) | -- Create the first and last line for the board
                     (21 < length list && length list <= 24)
                     || length list <= 3 = do
                                              putStrLn $
                                               createLine ((c,m):(take 2 bs))
                                               " --------------------- " ""
                                              printB $ drop 2 bs
                   | -- Create first and last line for the middle square
                     (18 < length list && length list <= 21)
                     || (3 < length list && length list <= 6)
                     = do
                          putStrLn $ "|     "
                                   ++ createLine ((c,m):(take 2 bs))
                                   " --------------- "
                                   "     |"
                          printB $ drop 2 bs
                   | -- Create the first and last line for the smallest square
                     (15 < length list && length list <= 18)
                     || (6 < length list && length list <= 9)
                     = do
                          putStrLn $ "|     |     "
                                   ++ createLine ((c,m):(take 2 bs))
                                      " --------- "
                                      "     |     |"
                          printB $ drop 2 bs
                   | -- Create the line in the middle of the board
                     otherwise = do
                                   putStr $ createLine ((c,m):(take 2 bs))
                                             " --- "
                                             "                       "
                                   putStrLn $ createLine (take 3 (drop 2 bs))
                                               " --- "
                                               ""
                                   printB $ drop 5 bs
  where list = (c, m):bs
        -- Create lines for output out of an array and some formated strings
        createLine (s:[]) dash after = getCase (fst s) ++ after
        createLine (s:ss) dash after = getCase (fst s) ++ dash ++ createLine ss dash after


getCase :: Case -> [Char]
getCase c = case c of
                      Case NineMen.Red   -> "H"
                      Case NineMen.Black -> "C"
                      _                  -> "°"

gameLoop :: Board -> IO()
gameLoop b | win       = do showWin
           | lose      = do showLose
           | otherwise = do 
                    printBoard b
                    putStrLn "Enter two numbers : which token move, enter key then the position"
                    if isNothing newBoard 
                        then gameLoop b 
                        else if alignements 
                                 then do
                                    putStrLn "Enter a token to remove : " 
                                    gameLoop (removeChoosenToken $ fromJust newBoard) 
                                 else gameLoop $ fromJust newBoard
    where 
        win                     = winner b == Just NineMen.Red
        lose                    = winner b == Just NineMen.Black
        showWin                 = putStrLn "Win Red"
        showLose                = putStrLn "Win Black"
        moveFrom                = askPosition
        moveTo                  = askPosition
        (newBoard, alignements) = createAlignement b moveFrom moveTo


removeChoosenToken :: Board -> Board
removeChoosenToken b =  
    let newBoard = removeToken b askPosition in
    if isNothing newBoard 
        then removeChoosenToken b 
        else fromJust newBoard

askPosition :: Int 
askPosition = let p = fromIOPosition (readInt) in
              if p == -1 
                then askPosition 
                else p

readInt :: IO Int
readInt = do inp <- getLine
             case reads inp of
               ((a,t1):_) | all isSpace(t1) -> return a
               _ -> return (-1)


fromIOPosition :: IO Int -> Int
{-# NOINLINE fromIOPosition #-} 
fromIOPosition i = let pos = unsafePerformIO i in
                if pos >= 0 && pos < 24 
                    then pos 
                    else -1