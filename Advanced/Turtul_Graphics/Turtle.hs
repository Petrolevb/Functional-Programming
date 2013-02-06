-- | The turtle graphic library
--   provide a way of creating programs that will be draw in a graphic window
module Turtle (
  -- * The turtle type(s)
  -- Non-exhausive list of possible types: Turtle, Program, Action, Operation
    Program
    , Turtle (getColor)
    , startingProgram
    , Action
    , Operation
  -- * Relative to the datas
    , getPos
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
    , decreaseLife
    , removeLife

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
data Turtle = Turtle {
                pos :: Position, angle :: Double, 
                getColor :: Color, pen :: Bool, 
                life :: Int
                     }
    deriving Show


getPos :: Turtle -> Position
getPos = pos

startingTurtle :: Turtle
startingTurtle = Turtle (0, 0) 0 (0, 0, 0) False (-1)

-- | The type of a complete program
--   The turle and the interface stored
type Program = [Action]

startingProgram :: Program
startingProgram = [(Start, startingTurtle)]

-- | An action is either :
--   Move, Turn, Draw, Undraw or Die
type Action = (Operation, Turtle)
data Operation =    Start | 
                    Move     | Turn       | 
                    Color    | ChangeDraw | 
                    GiveLife | Die
    deriving (Show, Eq)

-- | Move the turtle forward
forward  :: Program -> Double -> Program
-- | Rotate the turtle to the right
right    :: Program -> Double -> Program
-- | Move the turtle backward
--   rotating it by 180° and then move forward
backward :: Program -> Double -> Program
-- | Rotate the turtle to the left
--   rotating to the right for 360-'value'
left     :: Program -> Double -> Program
-- | Change the color of the turtle
color    :: Program -> Color -> Program
-- | Avoid the turtle to draw for until the pen is down again
penup    :: Program -> Program
-- | Allow the turtle to draw
pendown  :: Program -> Program
-- | Kill the turtle
die      :: Program -> Program
-- | Set the lifespan of the turtle, it will die when that time reach 0
lifespan :: Program -> Int -> Program
-- | Repeat a number of time the program
times    :: Program -> Int -> Program
-- | Run the program forever
forever  :: Program -> Program
-- | Stops the turtle
nothing  :: Program -> Program

-- a function will "take" the turtle of the program, then apply the action
-- and "build" the new program from it

forward actions len | checkLife turtle (ceiling $ abs len)
                            = (Move, newturtle turtle len):actions
                    | otherwise 
                            = die $ 
                              forward actions (fromIntegral $ life turtle)
    where turtle = snd $ head actions
          newturtle tur len
             = removeLife (
               Turtle (movePosition (pos tur) (angle tur) len) (angle tur)
                      (getColor tur) (pen tur) (life tur)
                          ) (ceiling len)
backward actions le = forward actions (-le)

right actions ang   | checkLife turtle 1
                            = (Turn, newturtle turtle ang):actions
                    | otherwise = die actions
    where turtle = snd $ head actions
          newturtle tur ang 
             = decreaseLife $
               Turtle (pos tur) (angle tur + ang)
                      (getColor tur) (pen tur) (life tur)
left     actions  ang = right actions (360 - ang)


color actions col   = (Color, newcol turtle col):actions
    where turtle = snd $ head actions
          newcol tur col 
             = Turtle (pos tur) (angle tur) col (pen tur) (life tur)

penup    actions    = (ChangeDraw, newturtle turtle):actions
    where turtle = snd $ head actions
          newturtle tur 
             =  Turtle (pos tur) (angle tur) (getColor tur) False (life tur)

pendown  actions    = (ChangeDraw, newturtle turtle):actions    
    where turtle = snd $ head actions
          newturtle tur 
             = Turtle (pos tur) (angle tur) (getColor tur) True (life tur)

die      actions    = (Die, newturtle turtle):actions     
    where turtle = snd $ head actions
          newturtle tur 
             = Turtle (pos tur) (angle tur) (getColor tur) False 0

lifespan actions li = (GiveLife, newturtle turtle):actions
    where turtle = snd $ head actions
          newturtle tur 
             = Turtle (pos tur) (angle tur) (getColor tur) (pen tur) li

times actions x | x == 0    = actions
                | otherwise = actions ++ times actions (x - 1)

forever actions  = actions ++ forever actions

nothing actions  = actions


-- | From a position and a direction, return the new position 
movePosition :: Position -> Double -> Double -> Position
movePosition (x, y) ang len = (x + len * cos ang, y + len * sin ang)


-- run :: Turtle -> (a-> Turtle) -> (Turtle, Turtle)
run tur action = (tur, action tur)

-- | Remove a number x of life to the turtle
removeLife :: Turtle -> Int -> Turtle
removeLife tur _ | life tur == -1 = tur
removeLife tur 0                  = tur
removeLife tur x                  = removeLife (decreaseLife tur) (x-1)

-- | Remove one life to the turtle
decreaseLife :: Turtle -> Turtle
decreaseLife tur | life tur > -1 = Turtle (pos tur) (angle tur) (getColor tur) (pen tur) (life tur - 1)
                 | otherwise     = tur

-- | Check  if the turtle has enough lifes
--   if the life is under 0, always True
checkLife :: Turtle -> Int -> Bool
checkLife tur x | life tur > -1 = life tur >= x
                | otherwise     = True


-- | Allow build sequential instructions
--   forward x y -=> right x y ...
(-=>) :: Program -> (Program -> a -> Program) -> Program
prog -=> next = (\a -> next prog a)

