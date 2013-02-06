module Main where

--import TurtleGraphics
import Turtle

-- main = runGraphical -- $ spiral (startingProgram) 0 91
main = runTextual turtle3
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
