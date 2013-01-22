import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
 
main :: IO ()
main = do
  (progname, _) <- getArgsAndInitialize
  initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
  initialWindowSize $= Size 500 500
  createWindow "Turtle Graphics"
  displayCallback $= display
  mainLoop
 
display :: IO ()
display = do
  clear [ ColorBuffer ]
  renderPrimitive Quads $ myCube
  flush

myCube =
  do
    color $ (Color3 (1.0::GLfloat) 0 0)
    vertex $ (Vertex3 (0  ::GLfloat) 0   0)
    vertex $ (Vertex3 (0.2::GLfloat) 0.1   0)
    vertex $ (Vertex3 (0.2::GLfloat) 0.4 0)
    vertex $ (Vertex3 (0  ::GLfloat) 0.3 0)
    color $ (Color3 (0::GLfloat) 1.0 0)
    vertex $ (Vertex3 (0   ::GLfloat) 0   0)
    vertex $ (Vertex3 (-0.2::GLfloat) 0.1   0)
    vertex $ (Vertex3 (-0.2::GLfloat) 0.4 0)
    vertex $ (Vertex3 (0   ::GLfloat) 0.3 0)
    color $ (Color3 (0::GLfloat) 0 1.0)
    vertex $ (Vertex3 (0   ::GLfloat) 0.3   0)
    vertex $ (Vertex3 (-0.2::GLfloat) 0.4   0)
    vertex $ (Vertex3 (0   ::GLfloat) 0.5 0)
    vertex $ (Vertex3 (0.2 ::GLfloat) 0.4 0)    

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = map (\k -> (sin(2*pi*k/12),cos(2*pi*k/12),0.0)) [1..12]