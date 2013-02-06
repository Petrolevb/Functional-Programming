-- | The turtle graphic library
--   provide a way of creating programs that will be draw in a graphic window
module Turtle (
  -- * The turtle type(s)
  -- Non-exhausive list of possible types: Turtle, Program, Action, Operation
      Program
    , Turtle (getColor)
    , Action
    , Operation (Move, Die)
    , startingProgram
  -- * Relative to the datas
    , getPos
    , getPen
  -- * Primitive operations
    , forward
    , right
    , color
    , penup
    , pendown
    , die
    , lifespan
    , times
    , forever
    , nothing
  -- , (<|>)
    , (-=>)
  -- , ... 
    , decreaseLife
    , removeLife

  -- * Derived operations
    , backward
    , left
  
  -- * Run functions
  , runTextual
  --, run
  
  ) where


{- 
------- Comments over the assignements ------

We appologize about some features over this assignements
we know that our implementation cannot cover the function forever
nor than it can allow us to use the parralelization with <|>

Nevertheless, we already thought about how change the source code
to use these features. Currently, we're using a program as a list 
of Action, which disallow us to know which turtle are moving.
We shall change this to another kind of list, or to a list of
operations and parrameters, applied to a turtle. By this way,
the implementation of <|> will be just to cross the two lists.

About the forever, the problem is that we have a stack of instructions
instead of a list. By the list implementation of haskell, we're adding
each operations to the stack, then reverse this stack just before 
executing operations. We shall change this by adding all instructions
each time to the end of the list, which will allow the used of the function
forever. In addition to that, the changing of the code for the <|> operation
will also correct another problem of forever : we keep a turtle and an 
instruction in the Action. Such as if we use forever with forward, the
turtle won't go stright away but will spawn to the original point, go to
its direction, then come back to the previous point, then go, etc...
By using Operation and parameters, we will loop over this operation and the
parameters not over the turtles



-}




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

startingTurtle :: Turtle
startingTurtle = Turtle (0, 0) 0 (1.0, 1.0, 1.0) True (-1)

-- | The type of a complete program
--   The turle and the interface stored
type Program = [Action]

startingProgram :: Program
startingProgram = [(Start, startingTurtle)]

-- | An action is an Operation and the Turtle that result from this operation
type Action = (Operation, Turtle)

-- | Define the different operation to know what to do
data Operation =    Start    |
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
-- | Return the position of a turtle
getPos :: Turtle -> Position
-- | Return the state of the pen
getPen :: Turtle -> Bool

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

left   actions  ang = right actions (360 - ang)

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

getPos = pos

getPen = pen


-- | From a position and a direction, return the new position 
movePosition :: Position -> Double -> Double -> Position
movePosition (x, y) ang len = (x + len * cos (ang * pi / 180), 
                               y + len * sin (ang * pi / 180))

-- | Remove a number x of life to the turtle
removeLife :: Turtle -> Int -> Turtle
removeLife tur _ | life tur == -1 = tur
removeLife tur 0                  = tur
removeLife tur x                  = removeLife (decreaseLife tur) (x-1)

-- | Remove one life to the turtle
decreaseLife :: Turtle -> Turtle
decreaseLife tur | life tur > -1 =
                        Turtle (pos tur) (angle tur)
                               (getColor tur) (pen tur) (life tur - 1)
                 | otherwise     = tur

-- | Check  if the turtle has enough lifes
--   if the life is under 0, always True
checkLife :: Turtle -> Int -> Bool
checkLife tur x | life tur > -1 = life tur >= x
                | otherwise     = True


-- | Allow build sequential instructions
--   forward x y -=> right x y ...
(-=>) :: Program -> (Program -> a -> Program) -> a -> Program
prog -=> next = (\a -> next prog a)

-- | Textual explanation of what a turtle do
runTextual :: Program -> IO ()
runTextual actions = do
  let a = reverse actions
  runTextual' a

runTextual' :: Program -> IO ()
runTextual' (_:[]) = return ()
runTextual' ((_, stur):etur:ss) = do explainAction stur etur
                                     runTextual' (etur:ss)

explainAction :: Turtle -> Action -> IO ()
explainAction _ (Start , _) =  putStrLn "The turtle starts"
explainAction sturtle (Move, eturtle)
            | not(getPen sturtle)  = do
                  let t = text ++ " without drawing"
                  putStrLn t 
            | otherwise = do
                  putStrLn text
                  putStrLn $ "drawing a line with the color " ++ 
                    "r : " ++ show r ++ ", g : " ++ show g ++
                    ", b : " ++ show b
  where text = "The turtle moves from position x : " ++ show x1
                  ++ ", y : " ++ show y1 ++ " to the position x : " ++ show x2
                  ++ ", y : " ++ show y2
        (x1, y1) = getPos sturtle
        (x2, y2) = getPos eturtle
        coor = [(x1,y1,0),(x2,y2,0)]
        (r,g,b) = getColor sturtle
explainAction sturtle (Turn, eturtle) = putStrLn text
  where a1 = angle sturtle
        a2 = angle eturtle
        a  = a2 - a1
        text = "The turtle turned " ++ show a ++ " degree"
explainAction _ (Color, tur) = putStrLn text
  where text = "The turtle changes the color to r : " ++ show r
               ++ ", g : " ++ show g ++ ", b : " ++ show b
        (r,g,b) = getColor tur
explainAction sturtle (ChangeDraw, eturtle) = putStrLn text
  where text = "The turtle puts the pen " ++ change sturtle eturtle
        change s e | pen s && not (pen e) = "up"
                   | otherwise = "down"
explainAction sturtle (GiveLife, eturtle) = putStrLn text
  where text = "The turtle gets " ++
                show (time sturtle eturtle) ++ " time unit to live"
        time s e | life s == -1 = life e
                 | otherwise = life e - life s
explainAction _ (Die, _) = putStrLn "The turtle dies"

