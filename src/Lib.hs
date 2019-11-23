module Lib
    ( raytracer
    ) where

import Vec
import Data.Maybe
import Data.List
import System.Random
import Data.Word (Word8)

data Color = RGB (V3 Double)
data Color24 = RGB24 (V3 Word8)

data Ray = Ray (V3 Double) (V3 Double)

data Shape = Sphere (V3 Double) Double

data Intersection = Intersection
    { intersectParam :: Double
    , intersectPoint :: V3 Double
    , intersectNormal :: V3 Double
    }

data Camera = Camera
    { cameraOrigin :: V3 Double
    , cameraLowerLeftCorner :: V3 Double
    , cameraHorizontal :: V3 Double
    , cameraVertical :: V3 Double
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
        xs -> Just $ minimumBy (\a b -> compare (intersectParam a) (intersectParam b)) xs

raysFromPixel :: Camera -> (Int, Int) -> [(Double, Double)] -> (Int, Int) -> [Ray]
raysFromPixel c (width, height) offsets (x, y) = let
    x' = fromIntegral x
    y' = fromIntegral y
    width' = fromIntegral width
    height' = fromIntegral height
    toUV (a, b) (o, o') = ((a + o) / width', (b + o') / height')
    uvs = fmap (toUV (x', y')) offsets
    ray (u, v) = Ray origin (corner + u .* horizontal + v .* vertical - origin)
        where
            origin = cameraOrigin c
            horizontal = cameraHorizontal c
            vertical = cameraVertical c
            corner = cameraLowerLeftCorner c
    in fmap ray $ uvs

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks 0 xs = [xs]
chunks n xs = chunk : chunks n rest where
    (chunk, rest) = splitAt n xs

raytracer :: IO ()
raytracer = let
        width = (200 :: Int)
        height = (100 :: Int)
        camera = Camera (V3 0 0 0) (V3 (-2) (-1) (-1)) (V3 4 0 0) (V3 0 2 0)
        spheres = [ (Sphere (V3 0 0 (-1)) 0.5), (Sphere (V3 0 (-100.5) (-1)) 100) ]
        raysPerPixel = (100 :: Int)
        -- Reversed, so that the order of pixels aligns with the order of pixels
        -- in the output file
        rows = [height - 1, height - 2 .. 0]
        cols = [0 .. width - 1]
        pixels = [ (x, y) | y <- rows, x <- cols ]
        innerTrace = (flip trace) spheres
        rays = raysFromPixel camera (width, height)
    in do
        rng <- getStdGen
        let (rng1, rng2) = split rng
        let offsets = chunks raysPerPixel $ zip (randoms rng1) (randoms rng2)
        let offsetRays = fmap rays offsets
        let rayGroups = zipWith ($) offsetRays pixels
        let colors = fmap (\rayGroup -> averageColor $ fmap innerTrace rayGroup) rayGroups
        let header = "P3\n200 100\n255\n"
        let content = concat $ (fmap colorToString colors)
        writeFile "/home/markus/out.ppm" (header ++ content)

colorToString :: Color -> String
colorToString c = (show r) ++ " " ++ (show g) ++ " " ++ (show b) ++ " "
    where (RGB24 (V3 r g b)) = toPPM c

toPPM :: Color -> Color24
toPPM (RGB v) = RGB24 $ toWord8 <$> (255.0 .* v) where
    toWord8 :: Double -> Word8
    toWord8 = fromIntegral . (round :: Double -> Integer)

averageColor :: [Color] -> Color
averageColor cs = RGB $ (combined /. (fromIntegral $ Data.List.length cs)) where
    combined = sum $ fmap (\(RGB v) -> v) cs

at :: Ray -> Double -> V3 Double
at (Ray o d) t = o + t .* d

backgroundGradient :: Ray -> Color
backgroundGradient (Ray _ d) = RGB $ (1 - t) .* white + t .* blue
    where
        white = V3 1 1 1
        blue = V3 0.5 0.7 1
        (V3 _ y _) = unit d
        t = 0.5 * (y + 1)

trace :: Ray -> [Shape] -> Color
trace r ss = 
    maybe background (normalToColor . intersectNormal) $ (closestIntersection r ss)
    where
        background = backgroundGradient r
        normalToColor n = RGB $ 0.5 .* ((V3 1 1 1) + n)

