type Radius = Float
type Side = Float
type Point = (Float, Float, Float)

data Region = -- Abstract syntax tree
    Sphere Radius
    | Cube Side
    | Translate Region Point
    | Outside Region 
    | Intersection Region Region
    | Union Region Region

data RegionAlgebra a =
  RAlg {
    ra_sphere  :: Radius -> a,
    ra_cube :: Side -> a,
    ra_translate  :: a -> Point -> a,
    ra_outside  :: a -> a,
    ra_intersection  :: a -> a -> a,
    ra_union :: a -> a -> a
  }

foldRegion :: RegionAlgebra a -> Region -> a
foldRegion alg (Sphere r)       = ra_sphere alg r
foldRegion alg (Cube s)         = ra_cube alg s
foldRegion alg (Translate r p)  = ra_translate alg (foldRegion alg r) p
foldRegion alg (Outside r)      = ra_outside alg (foldRegion alg r)
foldRegion alg (Intersection r1 r2) = ra_intersection alg (foldRegion alg r1) (foldRegion alg r2)
foldRegion alg (Union r1 r2) = ra_union alg (foldRegion alg r1) (foldRegion alg r2)


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
