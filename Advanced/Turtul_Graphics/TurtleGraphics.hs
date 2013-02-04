-- | A graphical run function for the turtle DSL
module TurtleGraphics (runGraphical) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit

import Turtle

-- | Draw a 2D cube representing a 3D cube,
--   in 2 different places
runGraphical :: IO ()
runGraphical = do
  (progname, _) <- getArgsAndInitialize
  initialWindowSize $= Size 800 800
  createWindow "Turtle Graphics"
  displayCallback $= display
  mainLoop

-- | Function that will draw a new line in the program
--   Sketch for the real function runGraphicals
runGraphical' :: [(GLfloat,GLfloat,GLfloat)] -> IO()
runGraphical' a = do
  renderPrimitive Lines $ mapM_ (\(x, y, z)->vertex$Vertex3 x y z) a
  flush
 
display :: IO ()
display = do
  clear [ ColorBuffer ]
  color (Color3 (1.0::GLfloat) 1.0 0)
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
    color (Color3 (1.0::GLfloat) 0 0)
    vertex (Vertex3 (0  ::GLfloat) 0   0)
    vertex (Vertex3 (0.2::GLfloat) 0.1 0)
    vertex (Vertex3 (0.2::GLfloat) 0.4 0)
    vertex (Vertex3 (0  ::GLfloat) 0.3 0)
    color (Color3 (0::GLfloat) 1.0 0)
    vertex (Vertex3 (0   ::GLfloat) 0   0)
    vertex (Vertex3 (-0.2::GLfloat) 0.1 0)
    vertex (Vertex3 (-0.2::GLfloat) 0.4 0)
    vertex (Vertex3 (0   ::GLfloat) 0.3 0)
    color (Color3 (0::GLfloat) 0 1.0)
    vertex (Vertex3 (0   ::GLfloat) 0.3 0)
    vertex (Vertex3 (-0.2::GLfloat) 0.4 0)
    vertex (Vertex3 (0   ::GLfloat) 0.5 0)
    vertex (Vertex3 (0.2 ::GLfloat) 0.4 0)
