module Samplers where


import CoreMath
import CoreTypes


data Sampler = Sampler
        {
            samples :: [Point3],
            disk_samples :: [Point3],
            hemisphere_samples :: [Point3]
        }


map_sample :: Point3 -> Float -> Point3
map_sample point@(x, y, z) exp = (sin_theta * cos_phi, sin_theta * sin_phi, cos_theta)
    where
        cos_phi = cos (2.0 * pi * x)
        sin_phi = sin (2.0 * pi * x)
        cos_theta = if y == 0 then (1.0 - y) ** (1.0 / (exp + 1.0)) else 0.0
        sin_theta = sqrt (1.0 - cos_theta * cos_theta)


map_hemisphere :: [Point3] -> Float -> [Point3]
map_hemisphere samples exp = [map_sample x exp | x <- samples]


map_disk :: [Point3] -> Float -> [Point3]
map_disk samples exp = [(x, y, 0.0) | (x, y, z) <- (map_hemisphere samples exp), x == x, y == y]


regular_samples :: Int -> [Point3]
regular_samples n = [pair x y | x <- [0..n], y <- [0..n]]
    where
        pair x y = ((fromIntegral x + 0.5) / fromIntegral n, (fromIntegral y + 0.5) / fromIntegral n, 0.0)


create_sampler :: (Int -> [Point3]) -> Int -> Float -> Sampler
create_sampler init_func samples_count exp = Sampler samples (map_disk samples exp) (map_hemisphere samples exp)
    where
        samples = init_func samples_count

