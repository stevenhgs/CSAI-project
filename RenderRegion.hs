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

drawUnion :: DisplayCallback -> DisplayCallback -> DisplayCallback
drawUnion dc1 dc2 = do
    dc1 
    dc2

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  renderPrimitive Points $ do
    color $ Color3 1 1 (1 :: GLfloat)
    vertex $ Vertex3 (-0.5) 0 (0 :: GLfloat)
  drawCube (0.5 :: GLdouble)
  flush