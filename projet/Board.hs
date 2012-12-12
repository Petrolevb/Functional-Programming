module Board where

import System.Console.ANSI
import NineMen
import Data.Maybe (isNothing, fromJust)
import GHC.IO

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
askPosition = let p = fromIOPosition (readLn :: IO Int) in
              if p == -1 
                then askPosition 
                else p


fromIOPosition :: IO Int -> Int
{-# NOINLINE fromIOPosition #-} 
fromIOPosition i = let pos = unsafePerformIO i in
                if pos >= 0 && pos < 24 
                    then pos 
                    else -1
