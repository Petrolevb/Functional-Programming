module Main where

import TurtleGraphics
import Turtle

main = runGraphical

spiral :: Double -> Double -> Program ()
spiral size angle | size > 100 = nothing 
                  | otherwise  = do
                         forward size
                         right angle
                         spiral (size + 2) angle

-- ^ Question : Can you use the lifespan command to define the example?
--       With the lifespan command, we make the turtle move until it dies.
--       We need to update the size before each forward
--       and spread the new value.