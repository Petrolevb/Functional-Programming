-- | proper module documentation here
module Turtle (
  -- * The turtle type(s)
  -- Non-exhausive list of possible types: Turtle, Program, Action, Operation

  -- * Primitive operations
      forward
    , right
    , backward
    , left
    , penup
    , pendown
  -- , (<|>)
  -- , ... 

  -- * Derived operations
  -- ...
  
  -- * Run functions
  -- runTextual :: Program -> IO ()
  -- ...
  
  ) where


type Programm -- the abstract type of a turtle program
  forward  :: Double -> Program
  right    :: Double -> Program
  backward :: Double -> Program -- uses forward with the opposite value
  left     :: Double -> Program -- uses right with the opposite value
  color    :: Double -> Double -> Double -> Program
  penup    :: Program
  pendown  :: Program
  die      :: Program
  lifespan :: Int -> Program
  times    :: Int -> Program
  forever  :: Program
