module Main where

import CoreMath
import CoreTypes
import ViewingSystem
import Tracer
import Samplers

import Control.Parallel
import Control.Parallel.Strategies

size = 1000 :: Int

make_pgm :: Int -> Int -> [ Color ] -> String
make_pgm width height xs = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ stringify(xs)
                  where stringify [] = ""
                        stringify ((x, y, z):xs) = show (round (x*255)) ++ " "
                                                 ++ show (round (y*255)) ++ " "
                                                 ++ show (round (z*255)) ++ " "
                                                 ++ stringify xs

viewplane_sampler = create_sampler (regular_samples) 8 1.0

view_plane = ViewPlane size size 1.0 viewplane_sampler


chessboard = ChessBoard (0.4, 0.8, 0.0) (0.0, 0.4, 0.8) 0.5


sphere1_material = Material  ( SingleColor (1.0, 0.0, 0.0))
                             ( SingleColor (1.0, 0.0, 0.0))
                               null_point_mapping
                               0.1 0.9 0.0 0.0 0.0 3.0 0.0

sphere2_material = Material  ( SingleColor (0.0, 1.0, 0.0))
                             ( SingleColor (0.0, 1.0, 0.0))
                               null_point_mapping
                               0.05 0.35 0.4 0.0 0.2 13.0 0.0

glass_material = Material    ( SingleColor (1.0, 1.0, 1.0))
                             ( SingleColor (1.0, 1.0, 1.0))
                               null_point_mapping
                               0.0 0.0 0.3 0.5 0.2 15 1.8

absolute_glass =  Material    ( SingleColor (1.0, 1.0, 1.0))
                              ( SingleColor (1.0, 1.0, 1.0))
                              null_point_mapping
                              0.0 0.0 0.2 0.8 0.0 15 2.0

absolute_mirror =  Material    ( SingleColor (1.0, 1.0, 1.0))
                               ( SingleColor (1.0, 1.0, 1.0))
                               null_point_mapping
                               0.0 0.0 0.2 0.0 0.8 15 1.8

plane_material = Material   chessboard
                            chessboard
                              clip_mapping
                              0.25 0.4 0.15 0.0 0.1 0.0 0.0

sun = DirectedLight (1.0, 1.0, 1.0) 1.5 (norm (2.0, -1.0, -1.0))
lamp = PointLight (1.0, 1.0, 1.0) 1.0 (3.0, 2.0, 2.0)

default_ambient = AmbientLight (1.0, 1.0, 1.0) 1.0

sphere1 = Sphere (5.0, 0.0, -1.0) 1.0
sphere2 = Sphere (5.0, 0.0, 1.0) 1.0
glass_sphere = Sphere (3.0, 0.0, 0.0) 0.5
absolute_refractor = Sphere (7.0, 0.0, -4.0) 1.0
absolute_reflector = Sphere (7.0, 0.0, 4.0) 1.0
top_sphere = Sphere (5.0, 4.0, 0.0) 2.0
plane = Plane (0.0, -1.0, 0.0) (0.0, 1.0, 0.0)

ambient_occlusion_test = create_ambient_occlusion (-0.5) 18 1.0

scene = Scene   [   (sphere1, sphere1_material),
                    (sphere2, sphere2_material),
                    (plane, plane_material),
                    (glass_sphere, glass_material),
                    (absolute_reflector, absolute_mirror),
                    (absolute_refractor, absolute_glass),
                    (top_sphere, absolute_mirror)
                 ]
                [sun] -- ++ ambient_occlusion_test)
                default_ambient
                (0.2, 0.3, 0.6)

camera = create_camera (1.0, 0.0, 0.0) (5.0, 0.0, 0.0) (0.0, 1.0, 0.0) 1.0 view_plane

result = (render_scene_dof camera 0.015 2.0 6 scene)

main = do
    let clipped = (using ((parMap rwhnf) (clip 0.0 1.0) result) (parList rdeepseq))
    (writeFile "result.pgm" (make_pgm size size clipped))