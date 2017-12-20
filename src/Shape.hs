module Shape
  ( Shape (Rectangle, Ellipse, RtTriangle, Polygon)
  , Radius
  , Side
  , Vertex
  , square
  , circle
  , disBetween
  , area
  ) where

data Shape
  = Rectangle Side Side
  | Ellipse Radius Radius
  | RtTriangle Side Side
  | Polygon [Vertex]
  deriving Show

type Radius = Float
type Side = Float
type Vertex = (Float, Float)

square :: Side -> Shape
square s = Rectangle s s

circle :: Radius -> Shape
circle r = Ellipse r r

rectangle :: Side -> Side -> Shape
rectangle s1 s2 = Rectangle s1 s2

rtTriangle :: Side -> Side -> Shape
rtTriangle s1 s2 = RtTriangle s1 s2

regularPolygon :: Int -> Side -> Shape
regularPolygon = undefined

area :: Shape -> Float
area (Rectangle s1 s2) = s1 * s2
area (Ellipse r1 r2) = pi * r1 * r2
area (RtTriangle s1 s2) = s1 * s2 / 2
area (Polygon (v1 : vs)) = polyArea vs
  where
    polyArea (v2 : v3 : vs') = triArea v1 v2 v3 + polyArea (v3 : vs')
    polyArea _               = 0

triArea :: Vertex -> Vertex -> Vertex -> Float
triArea v1 v2 v3 =
  let a = disBetween v1 v2
      b = disBetween v2 v3
      c = disBetween v3 v1
      s = (a + b + c) / 2
  in sqrt(s * (s - a) * (s - b) * (s - c))

disBetween :: Vertex -> Vertex -> Float
disBetween (x1, y1) (x2, y2) = sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
