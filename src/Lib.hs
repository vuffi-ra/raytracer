module Lib
    ( raytracer
    ) where

import Vec
import Data.Maybe
import Data.List

data Ray = Ray { origin :: V3 Double, direction :: V3 Double }

data Shape = Sphere { center :: V3 Double, radius :: Double }

data Intersection = Intersection
    { rayParam :: Double
    , point :: V3 Double
    , normal :: V3 Double
    }

-- A sphere with radius r around the origin is described by x*x + y*y + z^z = r *r.
-- A sphere with radius r at cx, cy, cz is described by (x - cx)^2 + (y - cy)^2 + (z - cz)^2 = r^2
-- This means a point p lies on a sphere with center c and radius r iff
--      (p -c) * (p - c) = r^2
-- A point in a ray p then lies on a sphere with center c and radius r iff
--      (p(t) - c) * (p(t) - c) = r^2 for any t
-- =>   (a + tb - c) * (a + tb - c) = r^2
-- =>   t^2 dot(b, b) + 2 t dot(b, a - c) + dot(a - c, a - c) - r^2 = 0
--
-- The general solution for this quadratic equation at^2 + bt + c is
--  (-b +- sqrt(b^2 - 4ac) / 2a
--  
-- We can find the number of intersection by looking at the discriminant b^2 - 4ac
-- case > 0: 2 Intersections
-- case = 0: Exactly one intersection
-- case < 0: No Intersections
--
-- Returns the t of the closest intersection
intersects :: Ray -> Shape -> Maybe Intersection
intersects r@(Ray origin direction) (Sphere center radius) = 
    if discriminant >= 0 && closestPointAlongRay > 0 then
        let
            point = r `at` closestPointAlongRay
            normal = unit $ (point - center)
        in
            Just $ Intersection closestPointAlongRay point normal
    else
        Nothing
    where
        toCenter = origin - center
        a = dot direction direction
        b = 2 * dot direction toCenter
        c = dot toCenter toCenter - radius**2
        discriminant = b**2 - 4 * a * c
        closestPointAlongRay = (-b - sqrt (b**2 - 4 * a * c)) / (2 * a)

intersections :: Ray -> [Shape] -> [Intersection]
intersections r ss = fromJust <$> (filter isJust $ (intersects r) <$> ss)

closestIntersection :: Ray -> [Shape] -> Maybe Intersection
closestIntersection r ss = 
    case (intersections r ss) of 
        [] -> Nothing
        xs -> Just $ minimumBy (\a b -> compare (rayParam a) (rayParam b)) xs

raytracer :: IO ()
raytracer = let
        width = (200 :: Int)
        height = (100 :: Int)
        lowerLeft = V3 (-2) (-1) (-1)
        horizontal = V3 4 0 0
        vertical = V3 0 2 0
        o = V3 0 0 0
        spheres = [ (Sphere (V3 0 0 (-1)) 0.5), (Sphere (V3 0 (-100.5) (-1)) 100) ]
        ray u v = Ray o (lowerLeft + u .* horizontal + v .* vertical)
        rays = 
            flip ray 
            <$> [ (fromIntegral c) / (fromIntegral height) | c <- [height - 1, height - 2..0]]
            <*> [ (fromIntegral c) / (fromIntegral width) | c <- [0..width - 1]] 
        out = concat $ fmap (colorToString . (flip colorFromRay) spheres) rays
    in do
        let header = "P3\n200 100\n255\n"
        writeFile "/home/markus/out.ppm" (header ++ out)

colorToString :: V3 Int -> String
colorToString (V3 r g b) = (show r) ++ " " ++ (show g) ++ " " ++ (show b) ++ " "

at :: Ray -> Double -> V3 Double
at (Ray o d) t = o + t .* d

backgroundGradient :: Ray -> V3 Int
backgroundGradient (Ray _ d) = fmap round $ (1 - t) .* white + t .* blue
    where
        white = V3 255 255 255
        blue = V3 120 200 255
        (V3 _ y _) = unit d
        t = 0.5 * (y + 1)

colorFromRay :: Ray -> [Shape] -> V3 Int
colorFromRay r ss = 
    maybe background (normalToColor . normal) $ (closestIntersection r ss)
    where
        background = backgroundGradient r
        normalToColor n = fmap (round . (*) 255) $ 0.5 .* ((V3 1 1 1) + n)

