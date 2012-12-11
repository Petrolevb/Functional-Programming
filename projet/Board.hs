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

gameLoop = undefined