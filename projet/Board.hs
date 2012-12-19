module Board where

import Data.Maybe (isNothing, fromJust, fromMaybe, isJust)
import Data.Char
import GHC.IO
import System.Console.ANSI
import NineMen

instance Show Board where
         show = printBoard

main = do phase1 newBoard NineMen.Red 9 9

-- Frist part when you put the tokens
phase1 :: Board -> Token -> Int -> Int -> IO()
phase1 b t 0 0 = phase2 b t
phase1 b t red black = case t of
                            NineMen.Red   -> do
                              newBoard <- makeBoard red
                              phase1 newBoard (nextToken t) (red-1) black
                            NineMen.Black -> do
                              newBoard <- makeBoard black
                              phase1 newBoard (nextToken t) red (black-1)
  where makeBoard i = do
          print b
          print t
          putStrLn $ "Enter the place where you want to place your token ("
                     ++ show i ++ " tokens left)"
          pos <- askPosition
          if isCaseFree b pos
             then let tmpBoard = changeCase b pos (Case t) in
                  if isJust $ isAlign tmpBoard pos
                     then do
                          putStrLn "Enter a token to remove : "
                          newBoard <- removeChoosenToken tmpBoard t
                          return newBoard
                     else return tmpBoard
             else makeBoard i


-- Second part when you move the tokens
phase2 :: Board -> Token -> IO()
phase2 b token | win       = showWin
               | lose      = showLose
               | otherwise = do
    putStr $ "\ESC[2J" ++ show b
    print token
    putStrLn "Enter two numbers : token to move, enter key then the position"
    (moveFrom, moveTo) <- doTwice
    if Just token /= tokenAt b moveFrom
      then phase2 b token
      else
          let (tmpBoard, alignments) = createAlignment b moveFrom moveTo in
          if isNothing tmpBoard 
             then phase2 b token
             else if alignments
                     then do
                          putStrLn "Enter a token to remove : "
                          newBoard <- removeChoosenToken (fromJust tmpBoard) token
                          phase2 newBoard (nextToken token)
                     else phase2 (fromJust tmpBoard) (nextToken token)
  where 
       win                     = winner b == Just NineMen.Red
       lose                    = winner b == Just NineMen.Black
       showWin                 = putStrLn "Red wins"
       showLose                = putStrLn "Black wins"
       doTwice                 = do
                                   a <- askPosition
                                   b <- askPosition
                                   return (a,b)


nextToken :: Token -> Token
nextToken t = case t of
                   NineMen.Black -> NineMen.Red
                   _             -> NineMen.Black

-- Remove a token from the board
removeChoosenToken :: Board -> Token -> IO Board
removeChoosenToken b t = do
    pos <- askPosition
    if Just t /= tokenAt b pos
       then let newBoard = removeToken b pos in
            maybe (removeChoosenToken b t) return newBoard
       else do
              putStrLn "This is one of your token. Choose another one"
              removeChoosenToken b t

-- Get a position from the user
askPosition :: IO Int
askPosition = do
                putStr "Input :  "
                inp <- getLine
                case reads inp of
                  ((a,t1):_) | all isSpace t1 -> return a
                  _ -> do putStrLn "Incorrect input"
                          askPosition


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


-- Get back a string depending on what is on the case
getCase :: Case -> String
getCase c = case c of
                      Case NineMen.Red   -> "R"
                      Case NineMen.Black -> "B"
                      _                  -> "°"
