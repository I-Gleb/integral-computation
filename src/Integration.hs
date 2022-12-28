module Integration where

import GHC.Float (int2Double)

within :: Double -> [Double] -> Either String Double
within eps (x : y : xs) | abs (x - y) < eps = Right x
                        | otherwise = within eps (y : xs)
within _ xs = Left "Couldn't achieve precesision"

average :: [Double] -> Double
average a = sum a / int2Double (length a)

nextMidPoint :: (Double -> Double) -> Double -> Double -> (Int,  Double) -> (Int,  Double)
nextMidPoint f a b (n, previous) = (3 * n, (previous
                                            + (b - a) * average (map (\x -> f (a + int2Double x * (b - a) / int2Double n + (b - a) / 6 / int2Double n)) [0 .. n - 1])
                                            + (b - a) * average (map (\x -> f (a + int2Double x * (b - a) / int2Double n + (b - a) * 5 / 6 / int2Double n)) [0 .. n - 1])) / 3)

evalIntegralMidPoint :: (Double -> Double) -> Double -> Double -> Double -> Either String Double
evalIntegralMidPoint f a b eps = within eps (map snd (iterate (nextMidPoint f a b) (1, f ((a + b) / 2) * (b - a))))