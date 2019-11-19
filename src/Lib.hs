module Lib
    ( writeGradient
    ) where

import Vec

data Ray = Ray { origin :: V3 Double, direction :: V3 Double }

writeGradient :: IO ()
writeGradient = do
    let header = "P3\n200 200\n255\n"
    writeFile "/home/markus/out.ppm" (header ++ gradient)

gradient :: String
gradient = concat [line y ++ "\n" | y <- [1..200]]

line :: Int -> String
line y = concat [posToColor x y | x <- [1..200]]

posToColor :: Integral a => a -> a -> String
posToColor x y = r <> " " <> g <> " 50 "
    where 
        r = show $ (round $ ((255.0 :: Double) * (fromIntegral x / 200.0)) :: Integer)
        g = show $ (round $ ((255.0 :: Double) * (fromIntegral y / 200.0)) :: Integer)

