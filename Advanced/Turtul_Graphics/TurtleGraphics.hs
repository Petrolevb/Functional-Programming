-- | A graphical run function for the turtle DSL
module TurtleGraphics (runGraphical) where

import Control.Concurrent
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GUI

import Turtle as T

-- | Proving information to send to the graphical rendering
data ProgramState = ProgramState { prog :: T.Program, pause :: Bool}

-- | Initialize a window before drawing the program
runGraphical ::  T.Program -> IO ()
runGraphical prog = do
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  initialWindowSize $= Size 800 800
  (progname, _) <- getArgsAndInitialize

  state <- newIORef $ ProgramState
                      prog
                      False

  GUI.createWindow "Turtle Graphics"
  GUI.displayCallback $= run state
  GUI.keyboardMouseCallback $= Just (keyboardCallback state)
  GUI.mainLoop

run ::  IORef ProgramState -> IO ()
run state = do
  clear[ ColorBuffer ]
  st@(ProgramState (actions, args, turtle)
                   pause) <- readIORef state
  let a = reverse actions
  let arg = reverse args
  forever <- runGraphical' (a, arg, turtle) state
  case forever of
       True  -> do newState <- newIORef $ ProgramState newProg pause
                   run newState
                    where newProg = recalculate actions args (startProg turtle)
       False -> return ()

-- | Helper function for going throught the list of Action
runGraphical' :: T.Program -> IORef ProgramState -> IO Bool
runGraphical' ((Forever,_):_, _, _) _ = return True
runGraphical' (action:[], args, tur) state
              = do st@(ProgramState _ pause) <- readIORef state
                   if pause then
                        runGraphical'
                        ([action], args, tur) state
                     else
                        do drawAction action tur
                           return False
runGraphical' (action1:(action2, etur):ss, arg:args, tur) state
              = do st@(ProgramState _ pause) <- readIORef state
                   if pause then
                        runGraphical'
                        (action1:(action2, etur):ss, arg:args, tur) state
                     else
                        do drawAction action1 etur
                           runGraphical' ((action2,etur):ss, args, tur) state

keyboardCallback :: IORef ProgramState -> GUI.KeyboardMouseCallback
keyboardCallback state (GUI.Char p) GUI.Down _ _ = do
                     st@(ProgramState _ p) <- readIORef state
                     state `writeIORef` st{ pause = not p }
--                     renderer st $ state
keyboardCallback _ _ _ _ _ = return ()

-- | Draw a line if the action requires it
drawAction :: Action -> Turtle -> IO ()
drawAction (Move, sturtle) eturtle
                     | not(getPen sturtle)  = return ()
                     | otherwise = drawLine col coor
  where (x1, y1) = getPos sturtle
        (x2, y2) = getPos eturtle
        coor = [(x1/100,y1/100,0),(x2/100,y2/100,0)]
        col = getColor sturtle
drawAction (_, tur) _ = return ()


-- | Function that will draw a new line in the program
drawLine :: (Float, Float, Float) -> [(Float, Float, Float)] -> IO()
drawLine (r,g,b) a = do
  GUI.color (Color3 r g b)
  renderPrimitive Lines $ mapM_ (\(x, y, z)->vertex$Vertex3 x y z) a
  flush
  swapBuffers
  threadDelay 10000
