module Tracer where

import CoreMath
import CoreTypes
import LocalModels
import ViewingSystem
import Data.List (sort)
import Control.Parallel
import Control.Parallel.Strategies


max_distance = 10*8 :: Float
reflect_threshold = 0.01 :: Float
refract_threshold = 0.01 :: Float

------------------------------------------------------------------------------------------------------------------------

trace :: Scene -> Int -> Ray -> Color
trace scene depth ray@(pos, dir)
    | depth < 1 = (0, 0, 0)
    | null hits = background scene
    | otherwise = shade nearest_hit
        where
            hits = filter (\x -> (distance x) > threshold) (hit_figures scene ray depth)
            nearest_hit = head (sort hits)

------------------------------------------------------------------------------------------------------------------------

reflect :: Hit -> Color
reflect hit@(Hit pos _ ray@(_, dir) norm _ scene _ depth) = result_color
    where
        reflected_ray = (pos, reflect_dir dir norm)
        result_color = trace scene (depth - 1) reflected_ray

refract :: Hit -> Color
refract hit@(Hit pos _ ray@(_, dir) norm mtr scene _ depth)
    | vSqr refracted_dir == 0 = (0, 0, 0)
    | otherwise = result_color
        where
            refracted_dir = refract_dir dir norm (q mtr)
            refracted_ray = (pos, refracted_dir)
            result_color = trace scene (depth - 1) refracted_ray

------------------------------------------------------------------------------------------------------------------------

shade :: Hit -> Color
shade hit = ambient_color <+> diffuse_color <+> specular_color <+> reflected_color <+> refracted_color
    where
        scene = hit_scene hit
        ambient_color = (get_ambient_color hit) <@> (trace_shadow hit (ambient scene)) <*. ka (hit_mtr hit)
        diffuse_color = lambert_color hit
        specular_color = phong_blinn_color hit
        reflected_color = if kr (hit_mtr hit) > reflect_threshold then reflect hit <*. kr (hit_mtr hit) else (0, 0, 0)
        refracted_color = if kt (hit_mtr hit) > refract_threshold then refract hit <*. kt (hit_mtr hit) else (0, 0, 0)

------------------------------------------------------------------------------------------------------------------------

render_pixel :: [Ray] -> Int -> Scene -> Color
render_pixel rays depth scene =
        (foldr (<+>) (0, 0, 0) (map (trace scene depth) rays)) /> divider
    where
        divider = fromIntegral (length rays)

render_scene :: Camera -> Int -> Scene -> [Color]
render_scene camera@(Camera _ _ _ vp _ _ _ _) depth scene =
        parMap rdeepseq (\rays -> render_pixel rays depth scene) pixels
    where
        rows = [[fill_pixel camera x y | x <- [0..(width vp) - 1]] | y <- [0..(height vp) - 1]]
        pixels = foldr (++) [] (reverse rows)


render_scene_dof :: Camera -> Float -> Float -> Int -> Scene -> [Color]
render_scene_dof camera@(Camera _ _ _ vp _ _ _ _) r d depth scene =
        parMap rdeepseq (\x -> render_pixel x depth scene) pixels
    where
        rows = [[fill_pixel_dof camera r d x y | x <- [0..width (vp) - 1]] | y <- [0..height (vp) - 1]]
        pixels = foldr (++) [] (reverse rows)