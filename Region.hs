module Region where

type RadiusGS = Double
type SideGS = Double
type PointGS = (Double, Double, Double)

data Region = -- Abstract syntax tree
    SphereGS RadiusGS
    | CubeGS SideGS
    | TranslateGS Region PointGS
    | OutsideGS Region 
    | IntersectionGS Region Region
    | UnionGS Region Region

data RegionAlgebra a =
  RAlg {
    ra_sphere  :: RadiusGS -> a,
    ra_cube :: SideGS -> a,
    ra_translate  :: a -> PointGS -> a,
    ra_outside  :: a -> a,
    ra_intersection  :: a -> a -> a,
    ra_union :: a -> a -> a
  }