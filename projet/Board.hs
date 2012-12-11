module Board where

import NineMen

printBoard :: Board -> IO()
printBoard [] = do putStrLn "\n"
printBoard ((Case c, a),bs) | length ((c, a),bs) == 24 || (not $ isMod3((c, a),bs))   = do 
                                                               printCase c
                                                               printBoard bs
                            | isMod3((c, a),bs) = do
                                                                printCase c
                                                                putStrLn "\n"
                                                                printBoard bs
  where isMod3 b = length b `mod` 3 == 0
        printCase c = putStrLn $ case c of
                                 Case Red   -> " R "
                                 Case Black -> " B "
                                 _                -> "   "
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
