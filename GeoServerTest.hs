import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT


type OurRadius = Float
type Side = Float
type Point = (Float, Float, Float)

data Region = -- Abstract syntax tree
    OurSphere OurRadius
    | OurCube Side
    | OurTranslate Region Point
    | OurOutside Region 
    | OurIntersection Region Region
    | OurUnion Region Region

data RegionAlgebra a =
  RAlg {
    ra_sphere  :: OurRadius -> a,
    ra_cube :: Side -> a,
    ra_translate  :: a -> Point -> a,
    ra_outside  :: a -> a,
    ra_intersection  :: a -> a -> a,
    ra_union :: a -> a -> a
  }

foldRegion :: RegionAlgebra a -> Region -> a
foldRegion alg (OurSphere r)       = ra_sphere alg r
foldRegion alg (OurCube s)         = ra_cube alg s
foldRegion alg (OurTranslate r p)  = ra_translate alg (foldRegion alg r) p
foldRegion alg (OurOutside r)      = ra_outside alg (foldRegion alg r)
foldRegion alg (OurIntersection r1 r2) = ra_intersection alg (foldRegion alg r1) (foldRegion alg r2)
foldRegion alg (OurUnion r1 r2) = ra_union alg (foldRegion alg r1) (foldRegion alg r2)


-- inRegion interpretation
inRegionAlgebra :: RegionAlgebra (Point -> Bool)
inRegionAlgebra = RAlg {
  ra_sphere = \r (x, y, z) -> x^2 + y^2 + z^2 <= r^2,
  ra_cube = \s (x, y, z) -> abs x <= s/2 && abs y <= s/2 && abs z <= s/2,
  ra_translate = \r (x0, y0, z0) (x, y , z) -> r (x - x0, y - y0, z - z0),
  ra_outside = \r p -> not (r p),
  ra_intersection = \r1 r2 p -> r1 p && r2 p,
  ra_union = \r1 r2 p -> r1 p || r2 p
}

-- serialize interpretation
serialize :: Region -> String
serialize = undefined


main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop


main2 :: Region -> IO ()
main2 region = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= drawing region
  mainLoop

drawing :: Region -> DisplayCallback
drawing region = do
    clear [ ColorBuffer ]
    foldRegion displayAlgebra region
    flush



drawSquare :: GLfloat -> DisplayCallback
drawSquare size = do
  renderPrimitive Quads $ do
    color $ Color3 1 0 (0 :: GLfloat)
    vertex $ Vertex3 (size/2) (size/2) (0 :: GLfloat)
    vertex $ Vertex3 (size/2) (-(size/2)) (0 :: GLfloat)
    vertex $ Vertex3 (-(size/2)) (-(size/2)) (0 :: GLfloat)
    vertex $ Vertex3 (-(size/2)) (size/2) (0 :: GLfloat)

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  renderPrimitive Points $ do
    color $ Color3 1 1 (1 :: GLfloat)
    vertex $ Vertex3 (-0.5) 0 (0 :: GLfloat)
  drawSquare (0.5 :: GLfloat)
  flush


displayAlgebra :: RegionAlgebra DisplayCallback
displayAlgebra = RAlg {
  ra_cube = drawSquare
}