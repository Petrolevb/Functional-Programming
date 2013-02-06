-- | The turtle graphic library
--   provide a way of creating programs that will be draw in a graphic window
module Turtle (
  -- * The turtle type(s)
  -- Non-exhausive list of possible types: Turtle, Program, Action, Operation
    --Program
    Turtle
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
  --, runTextual :: Program -> IO ()
  --, run
  
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
data Turtle = Turtle {pos :: Position, angle :: Double, getColor :: Color, pen :: Bool, life :: Int}
-- type Turtle a = Action -> (a, Position, Orientation, Color, Bool)

startingTurtle :: Turtle
startingTurtle = Turtle (0, 0) 0 (0, 0, 0) False (-1)

-- | The type of a complete program
--   The turle and the interface stored
--data Program = P { turtle :: Turtle, prog :: IO () }
-- newtype Program = P { getTurtle :: Turtle }
{-
instance Monad Program where
  return  =  undefined
  p >>= k   =  undefined
-}

{-
-- | An action is either :
--   Move, Turn, Draw, Undraw or Die
data Action = Move | Turn | Draw | Undraw | Die
-}

-- | Move the turtle forward
forward  :: Turtle -> Double -> Turtle
-- | Rotate the turtle to the right
right    :: Turtle -> Double -> Turtle
-- | Move the turtle backward
--   rotating it by 180° and then move forward
backward :: Turtle -> Double -> Turtle
-- | Rotate the turtle to the left
--   rotating to the right for 360-'value'
left     :: Turtle -> Double -> Turtle
-- | Change the color of the turtle
color    :: Turtle -> Color -> Turtle
-- | Avoid the turtle to draw for until the pen is down again
penup    :: Turtle -> Turtle
-- | Allow the turtle to draw
pendown  :: Turtle -> Turtle
-- | Kill the turtle
die      :: Turtle -> Turtle
-- | Set the lifespan of the turtle, it will die when that time reach 0
lifespan :: Turtle -> Int -> Turtle
-- | Repeat a number of time the program
times    :: Int -> Turtle
-- | Run the program forever
forever  :: Turtle -> Turtle
-- | Stops the turtle
nothing  :: Turtle -> Turtle

-- a function will "take" the turtle of the program, then apply the action
-- and "build" the new program from it

forward  tur len = Turtle (movePosition (pos tur) (angle tur) len) (angle tur)
                          (getColor tur) (pen tur) (life tur)
right    tur ang = Turtle (pos tur) ((angle tur) + ang)
                          (getColor tur) (pen tur) (life tur)
backward tur len = forward (right tur 180) len
left     tur ang = right tur (360 - ang)
color    tur col = Turtle (pos tur) (angle tur) col (pen tur) (life tur)
penup    tur     = Turtle (pos tur) (angle tur) (getColor tur) False (life tur)
pendown  tur     = Turtle (pos tur) (angle tur) (getColor tur) True (life tur)
die      tur     = Turtle (pos tur) (angle tur) (getColor tur) False 0
lifespan tur li  = Turtle (pos tur) (angle tur) (getColor tur) (pen tur) li
times            = undefined
forever          = undefined
nothing tur      = tur


-- | From a position and a direction, return the new position 
movePosition :: Position -> Double -> Double -> Position
movePosition (x, y) ang len = ((x + len * (cos ang)), (y + len * (sin ang)))


-- run :: Turtle -> (a-> Turtle) -> (Turtle, Turtle)
run tur action = (tur, action tur)
