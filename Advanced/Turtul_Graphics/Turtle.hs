-- | proper module documentation here
module Turtle (
  -- * The turtle type(s)
  -- Non-exhausive list of possible types: Turtle, Program, Action, Operation
    Turtle ,
    Program

  -- * Primitive operations
    , forward
    , right
    , backward
    , left
    , penup
    , pendown
    , die
    , times
    , lifespan
    , forever
  -- , (<|>)
  -- , ... 

  -- * Derived operations
  -- ...
  
  -- * Run functions
  -- runTextual :: Program -> IO ()
  -- ...
  
  ) where

-- type Action      = Double -- Action may be a parameter to modify
-- | Coordinates x and y of the turtle
type Position    = (Integer, Integer)
-- | Orientation of the turtle around the axis z
type Orientation = Double
-- | Color of the turtle
type Color       = (Double, Double, Double)

{-
newtype Program p = P { getTurtle :: Turtle p }
-- The type of a complete program
type Turtle t = Action -> (t, Position, Orientation, Color, Bool)
-- the abstract type of a turtle, with its position, orientation, color
--      and if it's down or not
-}

-- | A turtle has
--     a position
--     an orientation
--     a color
--     knows if the pen is down (True) or up (False)
type Turtle = (Position, Orientation, Color, Bool)
-- | A program is a sequence of actions to be performed by a turtle
type Program = undefined
-- | An action is performed by a turtle
--   and the new state of the turtle is returned
type Action = Turtle -> Operation -> Maybe Turtle

-- | General type for the following functions
type Operation = a -> Program

-- | Move the turtle forward
forward  :: Double -> Program ()
right    :: Double -> Program ()
backward :: Double -> Program () -- uses forward with the opposite value
left     :: Double -> Program () -- uses right with a computed value
color    :: Color -> Program ()
penup    :: Program ()
pendown  :: Program ()
die      :: Program ()  -- We stop the turtle
lifespan :: Int -> Program ()
times    :: Int -> Program ()
forever  :: Program ()

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
