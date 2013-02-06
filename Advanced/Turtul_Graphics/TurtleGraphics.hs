-- | A graphical run function for the turtle DSL
module TurtleGraphics (runGraphical) where

import Control.Concurrent
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GUI

import Turtle as T

-- | Proving information to send to the graphical rendering
data ProgramState = ProgramState { actions :: T.Program}

-- | Initialize a window before drawing the program
runGraphical ::  T.Program -> IO ()
runGraphical actions = do
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  initialWindowSize $= Size 800 800
  (progname, _) <- getArgsAndInitialize

  state <- newIORef actions

  createWindow "Turtle Graphics"
  displayCallback $= run state
  mainLoop

run ::  IORef [Action] -> IO ()
run state = do
  clear[ ColorBuffer ]
  actions <- readIORef state
  let a = reverse actions
  runGraphical' a

-- | Helper function for going throught the list of Action
runGraphical' :: T.Program -> IO ()
runGraphical' [] = return ()
runGraphical' (_:[]) = return ()
runGraphical' ((_, stur):etur:ss) = do drawAction stur etur
                                       runGraphical' (etur:ss)

-- | Draw a line if the action requires it
drawAction :: Turtle -> Action -> IO ()
drawAction sturtle
              (Move, eturtle)
                     | not(getPen sturtle)  = return ()
                     | otherwise = do drawLine col coor
  where (x1, y1) = getPos sturtle
        (x2, y2) = getPos eturtle
        coor = [(x1/100,y1/100,0),(x2/100,y2/100,0)]
        col = getColor sturtle
drawAction _ (_, tur) = return ()


-- | Function that will draw a new line in the program
-- drawLine :: (VertexComponent a1, ColorComponent a) => (a, a, a) -> [(a1, a1, a1)] -> IO()
drawLine (r,g,b) a = do
  GUI.color (Color3 r g b)
  renderPrimitive Lines $ mapM_ (\(x, y, z)->vertex$Vertex3 x y z) a
  flush
  swapBuffers
