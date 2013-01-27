module Main where


import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Turtle
import TurtleGraphics 

main :: IO ()
main = do
  (progname, _) <- getArgsAndInitialize
  initialWindowSize $= Size 500 500
  createWindow "Turtle Graphics"
  displayCallback $= display
  mainLoop
 
display :: IO ()
display = do
  clear [ ColorBuffer ]
  preservingMatrix $
    do
       translate (Vector3 (0.5 ::GLfloat) 0 0)
       renderPrimitive Quads $ myCube
  preservingMatrix $
    do
       rotate 90 (Vector3 (0 ::GLfloat) 0 1)
       renderPrimitive Quads $ myCube
  flush

myCube =
  do
    color $ (Color3 (1.0::GLfloat) 0 0)
    vertex $ (Vertex3 (0  ::GLfloat) 0   0)
    vertex $ (Vertex3 (0.2::GLfloat) 0.1 0)
    vertex $ (Vertex3 (0.2::GLfloat) 0.4 0)
    vertex $ (Vertex3 (0  ::GLfloat) 0.3 0)
    color $ (Color3 (0::GLfloat) 1.0 0)
    vertex $ (Vertex3 (0   ::GLfloat) 0   0)
    vertex $ (Vertex3 (-0.2::GLfloat) 0.1 0)
    vertex $ (Vertex3 (-0.2::GLfloat) 0.4 0)
    vertex $ (Vertex3 (0   ::GLfloat) 0.3 0)
    color $ (Color3 (0::GLfloat) 0 1.0)
    vertex $ (Vertex3 (0   ::GLfloat) 0.3 0)
    vertex $ (Vertex3 (-0.2::GLfloat) 0.4 0)
    vertex $ (Vertex3 (0   ::GLfloat) 0.5 0)
    vertex $ (Vertex3 (0.2 ::GLfloat) 0.4 0)
