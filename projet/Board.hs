module Board where

import System.Console.ANSI
import NineMen


printBoard :: Board -> IO()
printBoard b = do putStr "\ESC[2J"
                  printB $ line b

printB [] = putStrLn ""
printB ((c, a):bs) | length list == 24
                     || length list == 15
                     || not (isMod3 list) = do 
                                              printCase c
                                              printB    bs
                   | otherwise = do
                                    putStrLn  ""
                                    printCase c
                                    printB    bs
  where list = (c, a):bs
        isMod3 b = length b `mod` 3 == 0
        printCase c = putStr $ case c of
                                    Case NineMen.Red   -> " R "
                                    Case NineMen.Black -> " B "
                                    _          -> "   "


getCase c = case c of
                      Case NineMen.Red   -> " R "
                      Case NineMen.Black -> " B "
                      _          -> "   " 


gameLoop :: Board -> IO()
gameLoop b | win       b = showWin
           | lose      b = showLose
           | otherwise   = 
                    printBoard b
                    putStrLn "Enter two numbers : which token move, enter key then the position"
                    let (newBoard, alignements) = createAlignement b moveFrom moveTo
                    if isNothing newBoard 
                        then gameLoop b 
                    else 
                        if alignements 
                            then gameLoop (removeChoosenToken newBoard) 
                        else gameLoop newBoard
    where 
        win      = winner b == Red
        lose     = winner b == Black
        showWin  = "Win Red"
        showLose = "Win Black"
        moveFrom = askPosition
        moveTo   = askPosition


removeChoosenToken :: Board -> Board
removeChoosenToken b = putStrLn "Which token do you want to remove ?"
                       let newBoard = removeToken b askPosition
                       if isNothing newBoard then removeChoosenToken b else
                       fromJust newBoard

askPosition :: Int 
askPosition = do 
    position <- read :: Int

fromIOPosition :: IO Int -> Int
fromIOPosition IO i | i >= 0 && i < 24 = i
                    | otherwise        = -1
