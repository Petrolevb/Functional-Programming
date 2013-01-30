module Main where

import TurtleGraphics
import Turtle

main = runGraphical

spiral :: Double -> Double -> Program ()
spiral size angle | size > 100 = die 
                  | otherwise  = do
                         forward size
                         right angle
                         spiral (size + 2) angle
                  {-
                            forward size
                            right angle
                            spiral (size + 2) angle
                            -}
-- ^ Question : Can you use the lifespan command to define the example?
--       With the choice in the representation of the time
--       we can repeat the program with updated value of 'size'
--       and produce the same result
