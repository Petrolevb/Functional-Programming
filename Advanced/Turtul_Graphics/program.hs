module Program
    ( forward
    , right
    , backward
    , left
    , penup
    , pendown
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