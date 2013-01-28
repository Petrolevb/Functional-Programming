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

data Action      = Move | Turn | Draw | Undraw | Die
type Position    = (Integer, Integer)
type Color       = (Double, Double, Double)
type Orientation = Double

newtype Program p = P { getTurtle :: Turtle p }
-- The type of a complete program
type Turtle t = Action -> (t, Position, Orientation, Color, Bool)
-- the abstract type of a turtle, with its position, orientation, color
--      and if it's down or not

forward  :: Double -> Program ()
right    :: Double -> Program ()
backward :: Double -> Program () -- uses forward with the opposite value
left     :: Double -> Program () -- uses right with the opposite value
color    :: Double -> Double -> Double -> Program ()
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
