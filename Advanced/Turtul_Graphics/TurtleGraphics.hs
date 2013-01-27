module TurtleGraphics where

import Turtle


spiral :: Double -> Double -> Program ()
spiral size angle | size > 100 = die 
                  | otherwise  = undefined 
                  {-
                            forward size
                            right angle
                            spiral (size + 2) angle
                            -}
