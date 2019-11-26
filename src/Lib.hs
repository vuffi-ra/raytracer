module Lib
    ( raytracer
    ) where

import Vec
import Data.Maybe
import System.Random
import Data.Word (Word8)
import Control.Monad (ap, replicateM)
import Control.Applicative (ZipList(..))
import Data.List (minimumBy)

data Ray = Ray (V3 Double) (V3 Double)

data Albedo = Albedo (V3 Double)
data Material = Diffuse Albedo | Reflective Albedo

data Shape = Sphere (V3 Double) Double Material

data Intersection = Intersection
    { intersectParam :: Double
    , intersectPoint :: V3 Double
    , intersectNormal :: V3 Double
    , intersectMaterial :: Material
    }

data Camera = Camera
    { cameraOrigin :: V3 Double
    , cameraLowerLeftCorner :: V3 Double
    , cameraHorizontal :: V3 Double
    , cameraVertical :: V3 Double
    }

data AntiAliasingPattern = Pattern [(Double, Double)]

newtype Rand a = Rand { runRand :: StdGen -> (a, StdGen) }

instance Functor Rand where
    fmap f (Rand r) = Rand $ (\g -> let (a, g') = r g in (f a, g'))

instance Applicative Rand where
    pure = return
    (<*>) = ap

instance Monad Rand where
    return x = Rand $ \g -> (x, g)
    x >>= y = Rand $ \g -> let (a, g') = runRand x g in runRand (y a) g'

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
intersects r@(Ray origin direction) (Sphere center radius material) = 
    if discriminant >= 0 && closestPointAlongRay > 0.001 then
        let
            point = r `at` closestPointAlongRay
            normal = unit $ (point - center)
        in
            Just $ Intersection closestPointAlongRay point normal material
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
intersections r ss = fromJust <$> (filter isJust$ intersects r <$> ss)


closestIntersection :: Ray -> [Shape] -> Maybe Intersection
closestIntersection r ss = 
    case (intersections r ss) of 
        [] -> Nothing
        xs -> Just $ minimumBy (\a b -> compare (intersectParam a) (intersectParam b)) xs


raysFromPixel :: Camera -> (Int, Int) -> AntiAliasingPattern -> (Int, Int) -> [Ray]
raysFromPixel c (width, height) (Pattern offsets) (x, y) = let
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


rand :: Random a => Rand a
rand = Rand $ random


randTuple :: Random a => Rand (a, a)
randTuple = (,) <$> rand <*> rand


randomAAPattern :: Int -> Rand AntiAliasingPattern
randomAAPattern n = fmap Pattern $ replicateM n randTuple


raytracer :: IO ()
raytracer = let
        width = (400 :: Int)
        height = (200 :: Int)
        camera = Camera (V3 0 0 0) (V3 (-2) (-1) (-1)) (V3 4 0 0) (V3 0 2 0)
        diffuse = Diffuse (Albedo (V3 0.5 0.5 0.5))
        metal = Reflective (Albedo (V3 1 1 1))
        spheres = [ (Sphere (V3 0 0 (-120)) 20) diffuse, (Sphere (V3 0 (-140.5) (-120)) 100 diffuse) ]
        raysPerPixel = (10 :: Int)
        -- Reversed, so that the order of pixels aligns with the order of pixels
        -- in the output file
        rows = [height - 1, height - 2 .. 0]
        cols = [0 .. width - 1]
        pixels = [ (x, y) | y <- rows, x <- cols ]
        rays = raysFromPixel camera (width, height)
    in do
        rng <- getStdGen
        let (aaPatterns, _) = runRand (replicateM (length pixels) (randomAAPattern raysPerPixel)) rng
        let rayGroups = rays <$> (ZipList aaPatterns) <*> (ZipList pixels)
        let colors = averageColor <$> traceRays spheres <$> rayGroups

        let header = "P3\n" ++ (show width) ++ " " ++ (show height) ++ "\n255\n"
        let content = concat $ (fmap (colorToString . gammaCorrection) colors)
        writeFile "/home/markus/out.ppm" (header ++ content)


colorToString :: V3 Double -> String
colorToString c = (show r) ++ " " ++ (show g) ++ " " ++ (show b) ++ " " where 
    V3 r g b = toPPM c
    toPPM :: V3 Double -> V3 Word8
    toPPM v = toWord8 <$> (255.0 .* v) where
        toWord8 :: Double -> Word8
        toWord8 = fromIntegral . (round :: Double -> Integer)


gammaCorrection :: V3 Double -> V3 Double
gammaCorrection = fmap sqrt


averageColor :: [V3 Double] -> V3 Double
averageColor vs = (sum vs) /. (fromIntegral $ length vs) where


at :: Ray -> Double -> V3 Double
at (Ray o d) t = o + t .* d


backgroundGradient :: Ray -> V3 Double
backgroundGradient (Ray _ d) = (1 - t) .* white + t .* blue
    where
        white = V3 1 1 1
        blue = V3 0.5 0.7 1
        (V3 _ y _) = unit d
        t = 0.5 * (y + 1)

--                          
--     v \           /      
--        \         /^      
--         \   ^   / .      
--          \  |n /  .      
--           \ | /   . 2 * b * n
--            \|/    .      
--   ---------->-----------
--             |\    ^     
--             | \   .     
--             |  \  .     
--             |   \ .     
--             |    \.     
--             |     \     
mirrorAlong :: V3 Double -> V3 Double -> V3 Double
mirrorAlong n x = x - (2 * b .* n) where b = dot x $ unit n


traceRays :: [Shape] -> [Ray] -> [V3 Double]
traceRays shapes = fmap (traceRay shapes)


traceRay :: [Shape] -> Ray -> V3 Double
traceRay = traceRay' 0


traceRay' :: Int -> [Shape] -> Ray -> V3 Double
traceRay' 5  _ r = backgroundGradient r
traceRay' depth ss r@(Ray _ d) = case closestIntersection r ss of
    Nothing -> backgroundGradient r
    Just (Intersection _ p n m) -> case m of
        Diffuse (Albedo albedo) -> 
            -- NOTE: This assumes a single hardcoded unidirectional light
            let
                lightDir = unit $ V3 0 0 (-1)
                lightColor = V3 1 1 1
                cosTheta = max (dot n (-lightDir)) 0
            in
                cosTheta .* (albedo /. pi) * lightColor
        Reflective (Albedo albedo) ->
            let
                reflected = Ray p (mirrorAlong n d)
                color = traceRay' (depth + 1) ss reflected
            in
                albedo * color

