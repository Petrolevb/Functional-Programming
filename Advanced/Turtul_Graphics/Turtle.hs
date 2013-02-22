-- | The turtle graphic library
--   provide a way of creating programs that will be draw in a graphic window
module Turtle (
  -- * The turtle type(s)
  -- Non-exhausive list of possible types: Turtle, Program, Action, Operation
      Program
    , Turtle (getColor)
    , Action
    , Operation (Move, Die, Forever, Pause)
    , startingProgram
  -- * Relative to the datas
    , getPos
    , getPen
    , isShown
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
    , (-=>)
  -- , (<|>)
  -- , ...

  -- * Derived operations
    , backward
    , left
  -- * Useful functions
    , recalculate
    , startProg
    , p
  
  -- * Run functions
    , runTextual
  --, run
  
  ) where


{- 
------- Comments over the assignments ------

We apologize about some features over this assignments
we know that our implementation cannot cover the function forever
nor than it can allow us to use the parallelization with <|>

Nevertheless, we already thought about how change the source code
to use these features. Currently, we're using a program as a list 
of Action, which disallow us to know which turtle are moving.
We shall change this to another kind of list, or to a list of
operations and parameters, applied to a turtle. By this way,
the implementation of <|> will be just to cross the two lists.

About the forever, the problem is that we have a stack of instructions
instead of a list. By the list implementation of haskell, we're adding
each operations to the stack, then reverse this stack just before 
executing operations. We shall change this by adding all instructions
each time to the end of the list, which will allow the used of the function
forever. In addition to that, the changing of the code for the <|> operation
will also correct another problem of forever : we keep a turtle and an 
instruction in the Action. Such as if we use forever with forward, the
turtle won't go straight away but will spawn to the original point, go to
its direction, then come back to the previous point, then go, etc...
By using Operation and parameters, we will loop over this operation and the
parameters not over the turtles

We also know that the "stepping" function implies to change the implementation
of the type Program. But our aim is now to perform a working program which
will simply works, not something full of error

-}

{- Print part -}

printTurtle :: Turtle -> String
printTurtle tur = "Turtle : " ++
                  "pos = " ++ (show $ pos tur) ++ ", " ++
                  "angle = " ++ (show $ angle tur)

instance Show Turtle where
         show = printTurtle

p a = putStrLn $ showP a

showP (actions, args, turtle) = "[\n"++ concatMap showA actions ++ "]"

showA :: Action -> String
showA (op, tur) = "(" ++ show op ++ ", " ++ show tur ++ ")\n"

--}



-- | Coordinates x and y of the turtle
type Position    = (Float, Float)
-- | Orientation of the turtle around the axis z
type Orientation = Float
-- | Color of the turtle
type Color       = (Float, Float, Float)

-- | A turtle has
--     a position, an orientation, a color
--     and knows if the pen is down (True) or up (False)
data Turtle = Turtle {
                pos :: Position, angle :: Float, 
                getColor :: Color, pen :: Bool, 
                life :: Int,
                shown :: Bool
                     }
--    deriving Show

startingTurtle :: Turtle
startingTurtle = Turtle (0, 0) 0 (1.0, 1.0, 1.0) True (-1) True

-- | The type of a complete program
--   The turle and the interface stored
type Program = ([Action], [Arg], Turtle)

-- | Empty program
startingProgram :: Program
startingProgram = ([(Start,startingTurtle)], [(0,0,(0,0,0))], startingTurtle)

-- | Create an empty program starting with a specific turtle
startProg :: Turtle -> Program
startProg tur = ([(Start,startingTurtle)], [(0,0,(0,0,0))], tur)

-- | An action is an Operation and the Turtle that result from this operation
type Action = (Operation, Turtle)
-- | The type of the arguments are Int, Float or Color
type Arg    = (Int, Float, Color)

-- | Define the different operation to know what to do
data Operation =    Start    |
                    Move     | Turn       | Pause |
                    Color    | ChangeDraw | ChangeShown |
                    GiveLife | Die        | Forever
    deriving (Show, Eq)

-- | Move the turtle forward
forward  :: Program -> Float -> Program
-- | Rotate the turtle to the right
right    :: Program -> Float -> Program
-- | Move the turtle backward
--   rotating it by 180° and then move forward
backward :: Program -> Float -> Program
-- | Rotate the turtle to the left
--   rotating to the right for 360-'value'
left     :: Program -> Float -> Program
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
-- | Put the program in pause, wait a continue from the user
pause :: Program -> Program
-- | Add a pause to the execution
step       :: Program -> Program
-- | Set the program in the stepping mode
stepping   :: Program -> Program
-- | Set the turtle as shown
showTurtle :: Program -> Program
-- | Set the turtle as hidden
hideTurtle :: Program -> Program
-- | Return the position of a turtle
getPos :: Turtle -> Position
-- | Return the state of the pen
getPen :: Turtle -> Bool
-- | Return if the turtle is shown
isShown :: Turtle -> Bool


forward (actions, args, turtle) len 
    | checkLife turtle (ceiling $ abs len)
        = ((Move, turtle):actions, (0,len,(0,0,0)):args, newturtle turtle len)
    | otherwise 
        = die $ 
              forward (actions, args, turtle) (fromIntegral $ life turtle)
    where newturtle tur len
             = removeLife (
               Turtle (movePosition (pos tur) (angle tur) len) (angle tur)
                      (getColor tur) (pen tur) (life tur) (shown tur)
               ) (ceiling len)

backward prog len = forward prog (-len)

right (actions, args, turtle) ang
    | checkLife turtle 1
        = ((Turn, turtle):actions, (0,ang,(0,0,0)):args, newturtle turtle ang)
    | otherwise = die (actions, args, turtle)
    where newturtle tur ang 
             = decreaseLife $
               Turtle (pos tur) (angle tur - ang)
                      (getColor tur) (pen tur) (life tur) (shown tur)

left prog ang = right prog (360 - ang)

color (actions, args, turtle) col
    = ((Color, turtle):actions, (0,0,col):args, newturtle turtle col)
    where newturtle tur col
             = Turtle (pos tur) (angle tur) 
                      col (pen tur) (life tur)
                      (shown tur)

penup (actions, args, turtle)
    = ((ChangeDraw, turtle):actions, (0,0,(0,0,0)):args, newturtle turtle)
    where newturtle tur
             =  Turtle (pos tur) (angle tur) 
                       (getColor tur) False (life tur)
                       (shown tur)

pendown (actions, args, turtle)
    = ((ChangeDraw, turtle):actions, (0,0,(0,0,0)):args, newturtle turtle)
    where newturtle tur
             =  Turtle (pos tur) (angle tur) 
                       (getColor tur) True (life tur)
                       (shown tur)

die (actions, args, turtle)
    = ((Die, turtle):actions, (0,0,(0,0,0)):args, newturtle turtle)
    where newturtle tur
             = Turtle (pos tur) (angle tur) 
                      (getColor tur) False 0
                      (shown tur)

lifespan (actions, args, turtle) li
    = ((GiveLife, turtle):actions, (li,0,(0,0,0)):args, newturtle turtle)
    where newturtle tur
             = Turtle (pos tur) (angle tur) 
                      (getColor tur) (pen tur) li
                      (shown tur)

times (actions, args, turtle) x
    | x == 0    = (actions, args, turtle)
    | otherwise = nothing ((newactions ++ nextactions, newargs ++ nextargs, newturtle))
    where (newactions, newargs, newturtle)
            = recalculate (reverse actions) args ([(Start, nextturtle)], [], nextturtle)
          (nextactions, nextargs, nextturtle)
            = times (actions, args, turtle) (x-1)

-- | Withdraw the first action which is Start
recalculate :: [Action] -> [Arg] -> Program -> Program
recalculate [] [] (actions, args, turtle)
            = (newactions, args, turtle)
              where newactions = reverse $ drop 1 (reverse actions)
recalculate ((Start, _):actions) ((i, d, c):args) nactions
            = recalculate actions args nactions
recalculate ((Move, tur):actions) ((i, d, c):args) nactions
            = recalculate actions args newactions
              where newactions = forward nactions d
recalculate ((Turn, tur):actions) ((i, d, c):args) nactions
            = recalculate actions args newactions
              where newactions = right nactions d
recalculate ((Pause, _):actions) ((i, d, c):args) nactions
            = recalculate actions args nactions
--              where newactions = pause nactions
recalculate ((Color, tur):actions) ((i, d, c):args) nactions
            = recalculate actions args newactions
              where newactions = color nactions c
recalculate ((ChangeDraw, tur):actions) ((i, d, c):args) nactions
            | pen tur   = recalculate actions args (penup nactions)
            | otherwise = recalculate actions args (pendown nactions)
recalculate ((ChangeShown, tur):actions) ((i, d, c):args) nactions
            | shown tur = recalculate actions args (hideTurtle nactions)
            | otherwise = recalculate actions args (showTurtle nactions)
recalculate ((GiveLife, _):actions) ((i, d, c):args) nactions
            = recalculate actions args newactions
              where newactions = lifespan nactions i
recalculate ((Die, _):actions) ((i, d, c):args) nactions
            = recalculate actions args newactions
              where newactions = die nactions
recalculate ((Forever, _):actions) ((i, d, c):args) nactions
            = forever nactions



forever (actions, args, turtle)  = ((Forever, turtle):actions, args, turtle)

nothing actions  = actions

pause (actions, args, turtle) = ((Pause, turtle):actions, args, turtle)
    

getPos = pos
getPen = pen
isShown = shown

-- | From a position and a direction, return the new position 
movePosition :: Position -> Float -> Float -> Position
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
                               (shown tur)
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

step       = undefined
stepping   = undefined

showTurtle (actions, args, turtle)
    = ((ChangeShown, turtle):actions,(0,0,(0,0,0)):args, newturtle turtle)
    where newturtle tur
             = Turtle (pos tur) (angle tur) 
                      (getColor tur) (pen tur) (life tur)
                      True
hideTurtle (actions, args, turtle)
    = ((ChangeShown, turtle):actions,(0,0,(0,0,0)):args, newturtle turtle)
    where newturtle tur
             = Turtle (pos tur) (angle tur) 
                      (getColor tur) (pen tur) (life tur)
                      False

-- | Textual explanation of what a turtle do
runTextual :: Program -> IO ()
runTextual (actions, args, turtle) = do
  let a = reverse actions
  runTextual' (a, args, turtle)

runTextual' :: Program -> IO ()
runTextual' ([], _, _) = return ()
runTextual' (action:[], _, etur) = explainAction action etur
runTextual' (action:(a,etur):as, args, tur) = do
                                     explainAction action etur
                                     runTextual' ((a,etur):as, args, tur)

explainAction :: Action -> Turtle -> IO ()
explainAction (Start , _) _ =  putStrLn "The turtle starts"
explainAction (Move, sturtle) eturtle 
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
explainAction (Turn, sturtle) eturtle = putStrLn text
  where a1 = angle sturtle
        a2 = angle eturtle
        a  = a2 - a1
        text = "The turtle turns " ++ show a ++ " degree"
explainAction (Color, tur) _ = putStrLn text
  where text = "The turtle changes the color to r : " ++ show r
               ++ ", g : " ++ show g ++ ", b : " ++ show b
        (r,g,b) = getColor tur
explainAction (ChangeDraw, sturtle) eturtle = putStrLn text
  where text = "The turtle puts the pen " ++ change sturtle eturtle
        change s e | pen s && not (pen e) = "up"
                   | otherwise = "down"
explainAction (GiveLife, sturtle) eturtle = putStrLn text
  where text = "The turtle gets " ++
                show (time sturtle eturtle) ++ " time unit to live"
        time s e | life s == -1 = life e
                 | otherwise = life e - life s
explainAction (Die, _) _ = putStrLn "The turtle dies"
explainAction (Forever, _) _ = putStrLn "The turtle continues forever"
explainAction (_, _) _ = return ()

