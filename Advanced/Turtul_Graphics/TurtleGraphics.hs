-- | A graphical run function for the turtle DSL
module TurtleGraphics (runGraphical) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GUI
import Control.Concurrent
import System.Exit

import Turtle as T

initial :: IO ()
initial = do
  (progname, _) <- getArgsAndInitialize
  initialWindowSize $= Size 800 800
  createWindow "Turtle Graphics"
  displayCallback $= display
  mainLoop

{-
data Operation =    Start    |
                    Move     | Turn       |
                    Color    | ChangeDraw |
                    GiveLife | Die
type Action = (Operation, Turtle)
data Turtle = Turtle {pos :: Position, angle :: Double, getColor :: Color, pen :: Bool, life :: Int}
-}


-- | Draw a 2D cube representing a 3D cube,
--   in 2 different places
runGraphical ::  T.Program -> IO ()
runGraphical actions = do
  initial
  clear [ ColorBuffer ]
  let a = reverse actions
  runGraphical' a

runGraphical' :: T.Program -> IO ()
runGraphical' (_:[]) = return ()
runGraphical' ((_, stur):etur:ss) = do drawAction stur etur
                                       runGraphical' (etur:ss)

drawAction :: Turtle -> Action -> IO ()
drawAction sturtle
              (Move, eturtle)
                     | not(getPen sturtle)  = return ()
                     | otherwise = do drawLine col coor
  where (x1, y1) = getPos sturtle
        (x2, y2) = getPos eturtle
        coor = [(x1,y1,0),(x2,y2,0)]
        col = getColor sturtle
--        (r, g, b) = getColor sturtle
--        col = Color3 r g b
drawAction _ (_, tur) = return ()


-- | Function that will draw a new line in the program
-- drawLine :: (GLfloat, GLfloat, GLfloat) -> [(GLfloat,GLfloat,GLfloat)] -> IO()
drawLine (r,g,b) a = do
  GUI.color (Color3 r g b)
  renderPrimitive Lines $ mapM_ (\(x, y, z)->vertex$Vertex3 x y z) a
  flush

display :: IO ()
display = do
  clear [ ColorBuffer ]
  drawLine (1.0::GLfloat, 1.0, 1.0) [(0::GLfloat,0,0),((-1.0::GLfloat),0,0)]
  flush


{-
display :: IO ()
display = do
  clear [ ColorBuffer ]
  GUI.color (Color3 (1.0::GLfloat) 1.0 0)
  runGraphical' [(0,0,0),(1,0,0)]
  preservingMatrix $
    do
       translate (Vector3 (0.5 ::GLfloat) 0 0)
       renderPrimitive Quads myCube
  preservingMatrix $
    do
       translate (Vector3 (-0.5::GLfloat) 0 0)
       rotate 90 (Vector3 (0 ::GLfloat) 0 1)
       renderPrimitive Quads myCube
  flush

myCube =
  do
    GUI.color (Color3 (1.0::GLfloat) 0 0)
    vertex (Vertex3 (0  ::GLfloat) 0   0)
    vertex (Vertex3 (0.2::GLfloat) 0.1 0)
    vertex (Vertex3 (0.2::GLfloat) 0.4 0)
    vertex (Vertex3 (0  ::GLfloat) 0.3 0)
    GUI.color (Color3 (0::GLfloat) 1.0 0)
    vertex (Vertex3 (0   ::GLfloat) 0   0)
    vertex (Vertex3 (-0.2::GLfloat) 0.1 0)
    vertex (Vertex3 (-0.2::GLfloat) 0.4 0)
    vertex (Vertex3 (0   ::GLfloat) 0.3 0)
    GUI.color (Color3 (0::GLfloat) 0 1.0)
    vertex (Vertex3 (0   ::GLfloat) 0.3 0)
    vertex (Vertex3 (-0.2::GLfloat) 0.4 0)
    vertex (Vertex3 (0   ::GLfloat) 0.5 0)
    vertex (Vertex3 (0.2 ::GLfloat) 0.4 0)
-}