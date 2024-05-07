import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT


myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (0.1, 0.1, 0.1) ]

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop

drawSquare :: GLfloat -> DisplayCallback
drawSquare size = do
  renderPrimitive Quads $ do
    color $ Color3 1 0 (0 :: GLfloat)
    vertex $ Vertex3 0 0 (0 :: GLfloat)
    vertex $ Vertex3 0 (-size) (0 :: GLfloat)
    vertex $ Vertex3 size (-size) (0 :: GLfloat)
    vertex $ Vertex3 size 0 (0 :: GLfloat)

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  renderPrimitive Points $ do
    color $ Color3 1 1 (1 :: GLfloat)
    vertex $ Vertex3 (-0.5) 0 (0 :: GLfloat)
  drawSquare (5 :: GLfloat)
  flush