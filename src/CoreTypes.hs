module CoreTypes where


import CoreMath

threshold = 10**(-3) :: Float

------------------------------------------------------------------------------------------------------------------------

data Hit = None
         | Hit
         {
            hit_pos :: Point3,
            hit_local_pos :: Point3,
            hit_ray :: Ray,
            hit_norm :: Normal,
            hit_mtr :: Material,
            hit_scene :: Scene,
            distance :: Float,
            hit_depth :: Int
         }

------------------------------------------------------------------------------------------------------------------------

data Scene = Scene
        {
            figures :: [(Figure, Material)],
            lights :: [Light],
            ambient :: Light,
            background :: Color
        }

------------------------------------------------------------------------------------------------------------------------

instance Eq Hit where
    (==) None None = True
    (==) hit None = False
    (==) None hit = False
    (==) a b = distance a == distance b

instance Ord Hit where
    (>) a b = distance a > distance b
    (>=) a b = distance a >= distance b
    (<) a b = distance a < distance b
    (<=) a b = distance a <= distance b
    max a b = if (distance a) < (distance b) then b else a
    min a b = if (distance a) > (distance b) then b else a

------------------------------------------------------------------------------------------------------------------------

data Texture = SingleColor Color
             | ChessBoard Color Color Float
             deriving (Eq)

get_color :: Point2 -> Texture -> Color
get_color _ (SingleColor color) = color

get_color (x, y) (ChessBoard black white size)
    | mod (int_x + int_y) 2 == 0 = black
    | otherwise = white
        where (int_x, int_y) = (round x, round y)

------------------------------------------------------------------------------------------------------------------------

data Material = Material
                    {
                        diffuse_texture :: Texture,
                        specular_texture :: Texture,
                        mapping :: (Point3 -> Point2),
                        ka :: Float,
                        kd :: Float,
                        ks :: Float,
                        kt :: Float,
                        kr :: Float,
                        phong_power :: Float,
                        q :: Float
                    }

get_diffuse_color :: Hit -> Color
get_diffuse_color hit@(Hit _ lpos _ _ mtr _ _ _) = get_color ((mapping mtr) lpos) (diffuse_texture mtr)

get_specular_color :: Hit -> Color
get_specular_color hit@(Hit _ lpos _ _ mtr _ _ _) = get_color ((mapping mtr) lpos) (specular_texture mtr)

get_ambient_color :: Hit -> Color
get_ambient_color hit@(Hit _ lpos _ _ mtr _ _ _) = get_color ((mapping mtr) lpos) (diffuse_texture mtr)

null_point_mapping :: Point3 -> Point2
null_point_mapping _ = (0, 0)

clip_mapping :: Point3 -> Point2
clip_mapping (x, y, z) = (x, z)

------------------------------------------------------------------------------------------------------------------------

data Figure = Sphere
                 {
                     center :: Point3,
                     radius :: Float
                 }
            | Plane
                 {
                     center :: Point3,
                     normal :: Normal
                 }

------------------------------------------------------------------------------------------------------------------------

data Light = AmbientLight
                {
                    color :: Color,
                    ls :: Float
                }
           | PointLight
                {
                    color :: Color,
                    ls :: Float,
                    pos :: Point3
                }
           | DirectedLight
                {
                    color :: Color,
                    ls :: Float,
                    dir :: Vector
                }

------------------------------------------------------------------------------------------------------------------------

filter_transparent materials = filter (\x -> (kt x) > 0) materials

------------------------------------------------------------------------------------------------------------------------

hit_figures :: Scene -> Ray -> Int -> [Hit]
hit_figures scene ray depth =
    filter (\x -> x /= None && distance x > threshold) (map (hit scene ray depth) (figures scene))

------------------------------------------------------------------------------------------------------------------------

light_dir :: Hit -> Light -> Vector

light_dir _ (AmbientLight _ _) = (0, 0, 0)

light_dir hit (PointLight _ _ pos) = mkNormVec pos (hit_pos hit)

light_dir _ (DirectedLight _ _ dir) = dir

------------------------------------------------------------------------------------------------------------------------

trace_shadow :: Hit -> Light -> Color

trace_shadow hit (AmbientLight color ls) = color <*. ls

trace_shadow hit (PointLight color ls pos)
    | null hits = color <*. (ls / (dist * dist))
    | length transparent /= length materials = (0, 0, 0)
    | otherwise = color <*. (ls * (sum kts) / (fromIntegral (length transparent) * dist * dist))
        where
            ray = (hit_pos hit, norm (pos <-> hit_pos hit))
            dist = vLen (pos <-> hit_pos hit)
            hits = filter (\x -> distance x <= (dist - threshold)) (hit_figures (hit_scene hit) ray 0)
            materials = map hit_mtr hits
            transparent = filter_transparent materials
            kts = map kt transparent

trace_shadow hit (DirectedLight color ls dir)
    | null hits = color <*. ls
    | length transparent /= length materials = (0, 0, 0)
    | otherwise = color <*. (ls * (sum kts) / fromIntegral (length transparent))
            where
                ray = (hit_pos hit, dir <*. (-1.0)) :: Ray
                hits = hit_figures (hit_scene hit) ray 1
                materials = map hit_mtr hits
                transparent = filter_transparent materials
                kts = map kt transparent

------------------------------------------------------------------------------------------------------------------------

get_normal :: Figure -> Point3 -> Normal

get_normal sphere@(Sphere center _) point = norm (point <-> center)

get_normal plane@(Plane _ normal) _ = normal

------------------------------------------------------------------------------------------------------------------------

hit :: Scene -> Ray -> Int -> (Figure, Material) -> Hit
hit scene ray@(pos, dir) depth (sphere@(Sphere center radius), material)
        | roots == [] = None
        | otherwise = Hit hit_pos hit_normal ray hit_normal material scene (vLen (pos <-> hit_pos)) depth
        where
            d = pos <-> center
            roots = filter (> threshold) (solveq (dir <*.> dir, 2*(dir <*.> d), (d <*.> d) - radius^2))
            hit_pos = pos <+> (dir <*. minimum roots)
            hit_normal = get_normal sphere hit_pos

-------------------------------------------------------

hit scene ray@(pos, dir) depth (plane@(Plane center normal), material)
        | abs part < (10**(-5)) = None  -- parallel to a plane
        | otherwise = Hit hit_pos hit_pos ray normal material scene distance depth
            where
                part = dir <*.> normal
                distance = ((center <-> pos) <*.> normal) / part
                hit_pos = pos <+> (dir <*. distance)

------------------------------------------------------------------------------------------------------------------------

create_ambient_occlusion :: Float -> Int -> Float -> [Light]

create_ambient_occlusion y_dir count force =
        [DirectedLight (1.0, 1.0, 1.0) (force / fromIntegral count) dir | dir <- dirs]
    where
        muls = [fromIntegral i  * (2.0 * pi / fromIntegral count) | i <- [0..count]]
        dirs = [norm (cos n, y_dir, sin n) | n <- muls]


