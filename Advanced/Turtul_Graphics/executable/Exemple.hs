module Main where

import TurtleGraphics
import Turtle

main = runGraphical $ spiral startingProgram 0 91

test = runGraphical turtle3
  where turtle = forward startingProgram 0.5
        turtle1 = right turtle 90
        turtle2 = forward turtle1 0.5
        turtle3 = die turtle2


spiral :: Program -> Double -> Double -> Program
spiral tur size angle | size > 100 = nothing tur
                      | otherwise  = 
            spiral (forward tur size -=> right $ angle) (size + 2) angle
-- ^ Question : Can you use the lifespan command to define the example?
--       With the lifespan command, we make the turtle move until it dies.
--       We need to update the size before each forward
--       and spread the new value.


-- Draw a flower of the size in parameter
flower :: Program -> Double -> Program
flower actions size = times (square [head actions] size -=> right $ 10) 36

-- Draw a square of size in parameter
square :: Program -> Double -> Program
square actions size = times (forward [head actions] size -=> right $ 90) 4
