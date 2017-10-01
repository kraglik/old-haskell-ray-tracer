module LocalModels ( phong_color,
                     lambert_color,
                     phong_blinn_color
                    ) where

import CoreMath
import CoreTypes

------------------------------------------------------------------------------------------------------------------------

lambert_color :: Hit -> Color
lambert_color hit = diffuse_color <@> (foldr (<+>) (0, 0, 0) colors) <*. (kd (hit_mtr hit))
    where
        diffuse_color = get_diffuse_color hit
        shades = [trace_shadow hit x | x <- (lights (hit_scene hit))]
        powers = [(-1) * ((hit_norm hit) <*.> (light_dir hit x)) | x <- (lights (hit_scene hit))]
        pos_indices = [i | i <- [0..length powers - 1], (powers !! i) > 0]
        colors = [(powers !! i) .*> (shades !! i) | i <- pos_indices ]

------------------------------------------------------------------------------------------------------------------------

lights_powers :: Hit -> [Float]
lights_powers hit@(Hit _ _ ray@(_, dir) norm _ scene _ _) = powers
    where
        lights_reflected = [reflect_dir (light_dir hit x) norm | x <- (lights scene)]
        powers = map (\x -> (-1) .*> x <*.> dir) lights_reflected

pb_powers :: Vector -> Vector -> [Vector] -> [Float]
pb_powers dir normal light_dirs = map (\x -> (-1) .*> (norm (dir <+> x)) <*.> normal) light_dirs

------------------------------------------------------------------------------------------------------------------------

phong_color :: Hit -> Color
phong_color hit = specular_color <@> (foldr (<+>) (0, 0, 0) colors) <*. (ks (hit_mtr hit))
    where
        specular_color = get_specular_color hit
        shades = map (trace_shadow hit) (lights (hit_scene hit))
        powers = lights_powers hit
        pos_indices = [i | i <- [0..length powers - 1], (powers !! i) > 0]
        exp = phong_power (hit_mtr hit)
        colors = [((powers !! i) ** exp) .*> (shades !! i) | i <- pos_indices]

phong_blinn_color :: Hit -> Color
phong_blinn_color hit@(Hit pos _ ray@(_, dir) norm mtr scene _ depth) =
        specular_color <@> (foldr (<+>) (0, 0, 0) colors) <*. (ks mtr)
    where
        specular_color = get_specular_color hit
        shades = map (trace_shadow hit) (lights scene)
        powers = pb_powers dir norm (map (light_dir hit) (lights scene))
        pos_indices = [i | i <- [0..length powers - 1], (powers !! i) > 0]
        exp = phong_power mtr
        colors = [((powers !! i) ** exp) .*> (shades !! i) | i <- pos_indices]


