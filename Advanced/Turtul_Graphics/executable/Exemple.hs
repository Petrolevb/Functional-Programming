module Main where

import TurtleGraphics
import Turtle

-- main = runGraphical -- $ spiral (startingProgram) 0 91
main = runTextual turtle1
  where turtle = forward startingProgram 15
        turtle1 = die turtle


spiral :: Program -> Double -> Double -> Program
spiral = undefined
{-
spiral tur size angle | size > 100 = nothing tur
                      | otherwise  = spiral
                                        (right 
                                            (forward tur size)
                                         angle) 
                                      (size + 2) angle
-}                                      
{-
spiral :: Double -> Double -> Program ()
spiral size angle | size > 100 = nothing 
                  | otherwise  = do
                         forward size
                         right angle
                         spiral (size + 2) angle
-}
-- ^ Question : Can you use the lifespan command to define the example?
--       With the lifespan command, we make the turtle move until it dies.
--       We need to update the size before each forward
--       and spread the new value.
