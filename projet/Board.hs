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

gameLoop = undefined