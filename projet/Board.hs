module Board where

import System.Console.ANSI
import NineMen

printBoard :: Board -> IO()
printBoard b = do putStr "\ESC[2J"
                  printB $ line b

printB :: [(Case, a)] -> IO()
printB [] = putStrLn ""
printB ((c, m):bs) | (21 < length list && length list <= 24)
                     || length list <= 3 = do
                                              putStrLn $
                                               createLine ((c,m):(take 2 bs))
                                               " --------------------- " ""
                                              printB $ drop 2 bs
                   | (18 < length list && length list <= 21)
                     || (3 < length list && length list <= 6)
                     = do
                          putStrLn $ "|     "
                                   ++ createLine ((c,m):(take 2 bs))
                                   " --------------- "
                                   "     |"
                          printB $ drop 2 bs
                   | (15 < length list && length list <= 18)
                     || (6 < length list && length list <= 9)
                     = do
                          putStrLn $ "|     |     "
                                   ++ createLine ((c,m):(take 2 bs))
                                      " --------- "
                                      "     |     |"
                          printB $ drop 2 bs
                   | otherwise = do
                                   putStr $ createLine ((c,m):(take 2 bs))
                                             " --- "
                                             "                       "
                                   putStrLn $ createLine (take 3 (drop 2 bs))
                                               " --- "
                                               ""
                                   printB $ drop 5 bs
  where list = (c, m):bs
        createLine (s:[]) dash after = getCase (fst s) ++ after
        createLine (s:ss) dash after = getCase (fst s) ++ dash ++ createLine ss dash after

getCase :: Case -> [Char]
getCase c = case c of
                      Case NineMen.Red   -> "H"
                      Case NineMen.Black -> "C"
                      _                  -> "°"

gameLoop = undefined