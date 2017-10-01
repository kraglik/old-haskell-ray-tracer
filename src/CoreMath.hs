module CoreMath where

type Vector = (Float, Float, Float)

type Point3 = (Float, Float, Float)

type Point2 = (Float, Float)

type Normal = Vector

type Ray = (Point3, Vector)

type Color = Vector

(<^>) :: Vector -> Vector -> Vector
(<^>) (x1, y1, z1) (x2, y2, z2) = (y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2)

(<*.>) :: Vector -> Vector -> Float
(<*.>) (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

(<+>) :: Vector -> Vector -> Vector
(<+>) (a, b, c) (x, y, z) = (a + x, b + y, c + z)

(<->) :: Vector -> Vector -> Vector
(<->) (a, b, c) (x, y, z) = (a - x, b - y, c - z)

(<*.) :: Vector -> Float -> Vector
(<*.) (a, b, c) n = (a * n, b * n, c * n)

(/>) :: Vector -> Float -> Vector
(/>) (a, b, c) n = (a / n, b / n, c / n)

(.*>) :: Float -> Vector -> Vector
(.*>) n (a, b, c) = (a * n, b * n, c * n)

(</) :: Float -> Vector -> Vector
(</) n (a, b, c) = (a / n, b / n, c / n)

(<@>) :: Vector -> Vector -> Vector
(<@>) (a, b, c) (x, y, z) = (a * x, b * y, c * z)

vSqr :: Vector -> Float
vSqr vec = vec <*.> vec

vLen :: Vector -> Float
vLen vec = sqrt (vSqr vec)

norm :: Vector -> Vector
norm v@(a, b, c)
        | len < 10 ** (-9) = (0.0, 0.0, 0.0)
        | otherwise = (a / len, b / len, c / len)
        where
            len = vLen v

mkNormVec :: Point3 -> Point3 -> Vector
mkNormVec p1 p2 = norm $ p2 <-> p1

solveq :: (Float, Float, Float) ->[Float]
solveq (a,b,c)
     | d < 0 = []
     | d > 0 = [(- b - sqrt d)/(2*a), (- b + sqrt d)/(2*a)]
     | otherwise = [-b/(2*a)]
     where
        d = b*b - 4*a*c


reflect_dir :: Vector -> Normal -> Vector
reflect_dir dir n = norm (dir <+> refVec)
        where
            vDot = dir <*.> n
            refVec =  n <*. (vDot * (-2))


refract_dir :: Vector -> Normal -> Float -> Vector
refract_dir dir n q
            | nv > 0 = refract_dir dir ((-1) .*> n) a
            | d < 0 = (0, 0, 0)
            | otherwise = (a .*> dir) <-> (b .*> n)
                where
                    nv = n <*.> dir
                    a = 1.0 / q
                    d = 1.0 - (a * a) * (1.0 - nv * nv)
                    b = nv * a + sqrt d


clip :: Float -> Float -> Vector -> Vector
clip low high (x, y, z) = ( maximum [minimum [1.0, x], 0.0],
                            maximum [minimum [1.0, y], 0.0],
                            maximum [minimum [1.0, z], 0.0])
