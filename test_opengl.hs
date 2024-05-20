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

drawCube :: GLdouble -> DisplayCallback
drawCube side = do
  clearStencil $= 0
  stencilTest $= Enabled
  stencilFunc $= (Always, 1, 0xFFFFFFFF)
  stencilOp $= (OpReplace, OpReplace, OpReplace)
  color $ Color3 1 0 (0 :: GLfloat)
  renderObject Solid (Cube side)
  stencilTest $= Disabled

drawOutside :: DisplayCallback -> DisplayCallback
drawOutside cb = do
  stencilTest $= Enabled
  stencilFunc $= (Notequal, 1, 0xFFFFFFFF)
  cb
  stencilTest $= Disabled

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  drawOutside (drawCube 0.2)
  flush