module Lib
    ( writeGradient,
      rayGradient
    ) where

import Vec
import Data.List (intersperse)

data Ray = Ray { origin :: V3 Double, direction :: V3 Double }

writeGradient :: IO ()
writeGradient = do
    let header = "P3\n200 200\n255\n"
    writeFile "/home/markus/out.ppm" (header ++ gradient)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = chunk : chunksOf n rest
    where (chunk, rest) = splitAt n xs

rayGradient :: IO ()
rayGradient = let
        width = (200 :: Int)
        height = (100 :: Int)
        lowerLeft = V3 (-2) (-1) (-1)
        horizontal = V3 4 0 0
        vertical = V3 0 2 0
        origin = V3 0 0 0
        ray u v = Ray origin (lowerLeft + u .* horizontal + v .* vertical)
        rays = 
            flip ray 
            <$> [ (fromIntegral c) / (fromIntegral height) | c <- [height - 1, height - 2..0]]
            <*> [ (fromIntegral c) / (fromIntegral width) | c <- [0..width - 1]] 
        out = concat $ fmap (color . colorFromRay) rays
    in do
    let header = "P3\n200 100\n255\n"
    writeFile "/home/markus/out.ppm" (header ++ out)


gradient :: String
gradient = concat [line y ++ "\n" | y <- [1..200]]

line :: Int -> String
line y = concat [posToColor x y | x <- [1..200]]

posToColor :: Integral a => a -> a -> String
posToColor x y = r <> " " <> g <> " 50 "
    where 
        r = show $ (round $ ((255.0 :: Double) * (fromIntegral x / 200.0)) :: Integer)
        g = show $ (round $ ((255.0 :: Double) * (fromIntegral y / 200.0)) :: Integer)

color :: V3 Int -> String
color (V3 r g b) = (show r) ++ " " ++ (show g) ++ " " ++ (show b) ++ " "

colorFromRay :: Ray -> V3 Int
colorFromRay (Ray o d) = fmap round $ (1 - t) .* white + t .* blue
    where
        white = V3 255 255 255
        blue = V3 120 200 255
        t = 0.5 * ((y $ unit d) + 1)

