-- | The turtle graphic library
--   provide a way of creating programs that will be draw in a graphic window
module Turtle (
  -- * The turtle type(s)
  -- Non-exhausive list of possible types: Turtle, Program, Action, Operation
    Program

  -- * Primitive operations
    , forward
    , right
    , penup
    , pendown
    , die
    , lifespan
    , times
    , forever
    , nothing
  -- , (<|>)
  -- , ... 

  -- * Derived operations
    , backward
    , left
  
  -- * Run functions
  -- runTextual :: Program -> IO ()
  , run
  
  ) where

-- | Coordinates x and y of the turtle
type Position    = (Double, Double)
-- | Orientation of the turtle around the axis z
type Orientation = Double
-- | Color of the turtle
type Color       = (Double, Double, Double)


-- | A turtle has
--     a position, an orientation, a color
--     and knows if the pen is down (True) or up (False)
newtype Turtle = T {pos :: Position, angle :: Double, color :: Color, pen :: Bool}
-- type Turtle a = Action -> (a, Position, Orientation, Color, Bool)


-- | The type of a complete program
--   The turle and the interface stored
newtype Program = P { turtle :: Turtle, prog :: IO () }
-- newtype Program = P { getTurtle :: Turtle }

instance Monad Program where
  return  =  undefined
  p >>= k   =  undefined

{-
-- | An action is either :
--   Move, Turn, Draw, Undraw or Die
data Action = Move | Turn | Draw | Undraw | Die
-}

-- | Move the turtle forward
forward  :: Double -> Program ()
-- | Rotate the turtle to the right
right    :: Double -> Program ()
-- | Move the turtle backward
--   rotating it by 180° and then move forward
backward :: Double -> Program ()
-- | Rotate the turtle to the left
--   rotating to the right for 360-'value'
left     :: Double -> Program ()
-- | Change the color of the turtle
color    :: Color -> Program ()
-- | Avoid the turtle to draw for until the pen is down again
penup    :: Program ()
-- | Allow the turtle to draw
pendown  :: Program ()
-- | Kill the turtle
die      :: Program ()
-- | Set the lifespan of the turtle, it will die when that time reach 0
lifespan :: Int -> Program ()
-- | Repeat a number of time the program
times    :: Int -> (Double -> Program ()) -> Program ()
-- | Run the program forever
forever  :: (Double -> Program ()) -> Program ()
-- | Stops the turtle
nothing :: Program ()

-- a function will "take" the turtle of the program, then apply the action
-- and "build" the new program from it

forward  = undefined
right    = undefined
backward = undefined 
left     = undefined 
color    = undefined
penup    = undefined
pendown  = undefined
die      = undefined
lifespan = undefined
times    = undefined
forever  = undefined
nothing  = undefined

-- | Running a program is following what a turtle do
run :: Program a -> Turtle a
run = undefined
