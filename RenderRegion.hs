module RenderRegion where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Region


drawSphere :: GLdouble -> DisplayCallback
drawSphere radius = do
  color $ Color3 1 0 (0 :: GLfloat)
  renderObject Solid (Sphere' radius 40 40)

drawCube :: GLdouble -> DisplayCallback
drawCube side = do
  color $ Color3 1 0 (0 :: GLfloat)
  renderObject Solid (Cube side)

drawTranslate :: DisplayCallback -> PointGS -> DisplayCallback
drawTranslate cb (x, y, z) = do
  preservingMatrix $ do
    translate $ Vector3 x y z
    cb

drawOutside :: DisplayCallback -> DisplayCallback
drawOutside dc = do
  stencilTest $= Enabled
  colorMask $= Color4 Disabled Disabled Disabled Disabled
  stencilOp $= (OpReplace, OpReplace, OpReplace)
  stencilFunc $= (Always, 1, 0xFF)
  stencilMask $= 0xFF

  dc

  colorMask $= Color4 Enabled Enabled Enabled Enabled
  stencilFunc $= (Notequal, 1, 0xFF)
  stencilMask $= 0x00
  color (Color3 1.0 0.0 0.0 :: Color3 GLfloat)
  renderPrimitive Quads $ do
    vertex (Vertex2 (-1.0) (-1.0) :: Vertex2 GLfloat)
    vertex (Vertex2 1.0 (-1.0) :: Vertex2 GLfloat)
    vertex (Vertex2 1.0 1.0 :: Vertex2 GLfloat)
    vertex (Vertex2 (-1.0) 1.0 :: Vertex2 GLfloat)

  -- Disable stencil testing
  stencilTest $= Disabled
  

drawIntersection :: DisplayCallback -> DisplayCallback -> DisplayCallback
drawIntersection dc1 dc2 = do
  stencilTest $= Enabled
  colorMask $= Color4 Disabled Disabled Disabled Disabled
  stencilOp $= (OpReplace, OpReplace, OpReplace)
  stencilFunc $= (Always, 1, 0xFF)
  stencilMask $= 0xFF
  dc1
  stencilTest $= Enabled
  colorMask $= Color4 Enabled Enabled Enabled Enabled
  stencilFunc $= (Equal, 1, 0xFF)
  stencilMask $= 0x00
  dc2
  stencilTest $= Disabled


drawUnion :: DisplayCallback -> DisplayCallback -> DisplayCallback
drawUnion dc1 dc2 = do
    dc1 
    dc2

-- FOR TESTING
display :: DisplayCallback
display = do
  clear [ ColorBuffer, DepthBuffer, StencilBuffer]
  drawIntersection (drawTranslate (drawCube 0.2) (0.1, 0, 0)) (drawCube 0.2)
  flush

main :: IO()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, WithStencilBuffer]
  _window <- createWindow "GeoServer"
  clearColor $= Color4 0.0 0.0 0.0 1.0
  displayCallback $= display
  mainLoop
