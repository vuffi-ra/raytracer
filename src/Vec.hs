{-# LANGUAGE TypeFamilies #-}
module Vec
    (
    V3(..),
    Vec.norm,
    (.*),
    (/.),
    unit,
    dot,
    ) where

import Control.Applicative

data V3 a = V3 a a a deriving (Show)

instance Functor V3 where
    fmap f (V3 a b c)= V3 (f a) (f b) (f c)

instance Applicative V3 where
    pure a = V3 a a a
    V3 a b c <*> V3 d e f = V3 (a d) (b e) (c f)

instance Num a => Num (V3 a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (V3 a) where
    fromRational = pure . fromRational
    recip = fmap recip 

instance Floating a => Floating (V3 a) where
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh

(.*) :: Num a => a-> V3 a -> V3 a
(.*) = fmap . (*)

(/.) :: Fractional a => V3 a -> a -> V3 a
a /. b = recip b .* a

norm :: Floating a => V3 a -> a
norm a = sqrt (dot a a)

unit :: Floating a => V3 a -> V3 a
unit v = v /. (norm v)

dot :: Num a => V3 a -> V3 a -> a
dot (V3 a b c) (V3 a' b' c') = a * a' + b * b' + c * c'
    
