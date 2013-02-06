-- | A graphical run function for the turtle DSL
module TurtleGraphics (runGraphical) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Concurrent
import System.Exit

import Turtle

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
runGraphical ::  [Action] -> IO ()
runGraphical actions = do
  initial
  let a = reverse actions in
      let s = snd $ head a
      runGraphical' s (head $ tail a)

-- ((x2,y2), a2, c2, pen2, l2)
runGraphical' :: Turtle -> Action -> IO (Turtle)
runGraphical' sturtle
              (Move, eturtle)
                     | not(getPen sturtle)  = return eturtle
                     | otherwise = do drawLine col [(x1,y1,0),(x2,y2,0)]
                                      return eturtle
  where (x1, y1) = getPos sturtle
        (x2, y2) = getPos eturtle
        (r, g, b) = getColor sturtle
        col = Color3 r g b
runGraphical' _ (Die, tur) = return tur
runGraphical' _ (_, tur) = return tur
{-
runGraphical' (Turn, tur)       =
runGraphical' (Color, tur)      =
runGraphical' (ChangeDraw, tur) =
runGraphical' (GiveLife, tur)   =
-}


-- | Function that will draw a new line in the program
-- drawLine :: Color3 -> [(GLfloat,GLfloat,GLfloat)] -> IO()
drawLine col a = do
  color col
  renderPrimitive Lines $ mapM_ (\(x, y, z)->vertex$Vertex3 x y z) a
  flush

drawLine' (r,g,b) a = do
  color (Color3 r g b)
  renderPrimitive Lines $ mapM_ (\(x, y, z)->vertex$Vertex3 x y z) a
  flush

display :: IO ()
display = do
  clear [ ColorBuffer ]
 {-
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
  -}
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
