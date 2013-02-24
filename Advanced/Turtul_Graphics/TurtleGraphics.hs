-- | A graphical run function for the turtle DSL
module TurtleGraphics (runGraphical) where

import Control.Concurrent
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GUI

import Turtle as T

-- | Proving information to send to the graphical rendering
data ProgramState = ProgramState { prog :: T.Program}

-- | Initialize a window before drawing the program
runGraphical ::  T.Program -> IO ()
runGraphical prog = do
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  initialWindowSize $= Size 800 800
  (progname, _) <- getArgsAndInitialize

  state <- newIORef prog

  createWindow "Turtle Graphics"
  displayCallback $= run state
  mainLoop

run ::  IORef T.Program -> IO ()
run state = do
  clear[ ColorBuffer ]
  (actions, args, turtle) <- readIORef state
  let a = reverse actions
  let arg = reverse args
  forever <- runGraphical' (a, arg, turtle)
  case forever of
       True  -> do newState <- newIORef newProg
                   run newState
                    where newProg = recalculate actions args (startProg turtle)
       False -> return ()

-- | Helper function for going throught the list of Action
runGraphical' :: T.Program -> IO Bool
runGraphical' ((Forever,_):_, _, _) = return True
runGraphical' (_:[], _, _) = return False
runGraphical' (action1:(action2, etur):ss, arg:args, tur)
              = do drawAction action1 etur
                   runGraphical' ((action2,etur):ss, args, tur)

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
--  threadDelay 100000
