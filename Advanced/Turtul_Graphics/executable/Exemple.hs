module Main where

import TurtleGraphics
import Turtle

--main = runGraphical $ spiral startingProgram 0 91
main = runGraphical $ flower (color startingProgram (1.0,1.0,0.0)) 75

spiral :: Program -> Float -> Float -> Program
spiral tur size angle | size > 100 = nothing tur
                      | otherwise  = 
            spiral (forward tur size -=> right $ angle) (size + 2) angle
-- ^ Question : Can you use the lifespan command to define the example?
--       With the lifespan command, we make the turtle move until it dies.
--       We need to update the size before each forward
--       and spread the new value.


-- Draw a flower of the size in parameter
flower :: Program -> Float -> Program
flower actions size = times (square actions size -=> right $ 10) 36

-- Draw a square of size in parameter
square :: Program -> Float -> Program
square actions size = times (forward actions size -=> right $ 90) 4
