module ViewingSystem where

import CoreMath
import CoreTypes
import Samplers

------------------------------------------------------------------------------------------------------------------------

data Camera = Camera
        {
            camera_pos :: Point3,
            camera_target :: Vector,
            up_dir :: Vector,
            viewplane :: ViewPlane,
            viewplane_distance :: Float,
            camera_u :: Vector,
            camera_v :: Vector,
            camera_w :: Vector
        }

data ViewPlane = ViewPlane
        {
            width :: Int,
            height :: Int,
            pixel_size :: Float,
            sampler :: Sampler
        }

------------------------------------------------------------------------------------------------------------------------

create_camera :: Vector -> Vector -> Vector -> Float -> ViewPlane -> Camera
create_camera pos target up distance view_plane = Camera pos target up view_plane distance u v w
    where
        w = norm (pos <-> target)
        u = norm (up <^> w)
        v = norm (w <^> u)

init_ray_direction :: Camera -> Float -> Float -> Vector
init_ray_direction camera x y =
        norm ((x .*> u) <+> (y .*> v) <-> (viewplane_distance camera .*> w))
    where
        (u, v, w) = (camera_u camera, camera_v camera, camera_w camera)

init_square_samples :: (Float, Float) -> (Float, Float) -> [Vector] -> [Vector]
init_square_samples (w, h) (pos_x, pos_y) samples =
        [(x / hw + pos_x / hw, y / hh + pos_y / hh, 0) | (x, y, _) <- samples]
    where
        (hw, hh) = (w / 2, h / 2)

fill_pixel :: Camera -> Int -> Int -> [Ray]
fill_pixel camera x y =
        [(camera_pos camera, init_ray_direction camera _x _y) | (_x, _y, _) <- square_samples]
    where
        (w, h) = (fromIntegral (width (viewplane camera)), fromIntegral (height (viewplane camera)))
        (pos_x, pos_y) = (fromIntegral x - (w / 2), fromIntegral y - (h / 2))
        square_samples = init_square_samples (w, h) (pos_x, pos_y) (samples (sampler (viewplane camera)))


fill_pixel_dof :: Camera -> Float -> Float -> Int -> Int -> [Ray]
fill_pixel_dof camera@(Camera pos _ _ vp vpd u v w_dir) focal_r focal_d x y =
        zipWith (\new_pos point -> (new_pos, norm (point <-> new_pos))) poses points
    where
        (w, h) = (fromIntegral (width vp), fromIntegral (height vp))
        (pos_x, pos_y) = (fromIntegral x - (w / 2), fromIntegral y - (h / 2))
        square_samples = init_square_samples (w, h) (pos_x, pos_y) (samples (sampler vp))
        poses = [pos <+> (focal_r .*> norm((x .*> u) <+> (y .*> v)))| (x, y, _) <- disk_samples(sampler vp)]
        dirs = [init_ray_direction camera _x _y | (_x, _y, _) <- square_samples]
        points = [pos <+> ((((-1.0) / (dir <*.> w_dir)) * focal_d).*> dir) | dir <- dirs]


