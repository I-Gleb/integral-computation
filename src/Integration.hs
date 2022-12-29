module Integration where

import GHC.Float (int2Double)

within :: Double -> [Double] -> Either String Double
within eps (x : y : xs) | abs (x - y) < eps = Right y
                        | otherwise = within eps (y : xs)
within _ xs = Left "Couldn't achieve precesision"

average :: [Double] -> Double
average a = sum a / int2Double (length a)

nextMidPoint :: (Double -> Double) -> Double -> Double -> (Int,  Double) -> (Int,  Double)
nextMidPoint f a b (n, previous) = let dx = (b - a) / int2Double n in 
                                    (3 * n, (previous
                                                    + (b - a) * average (map (\x -> f (a + int2Double x * dx + dx / 6)) [0 .. n - 1])
                                                    + (b - a) * average (map (\x -> f (a + int2Double x * dx + dx * 5 / 6)) [0 .. n - 1])) / 3)

evalIntegralMidPoint :: (Double -> Double) -> Double -> Double -> Double -> Either String Double
evalIntegralMidPoint f a b eps = within eps (map snd (iterate (nextMidPoint f a b) (1, (f ((a + b) / 2)) * (b - a))))

nextTrapezoid :: (Double -> Double) -> Double -> Double -> (Int,  Double) -> (Int,  Double)
nextTrapezoid f a b (n, previous) = (2 * n, (previous
                                            + (b - a) * average (map (\x -> f (a + (int2Double x + 0.5) * (b - a) / int2Double n)) [0 .. n - 1])) / 2)

evalIntegralTrapezoid :: (Double -> Double) -> Double -> Double -> Double -> Either String Double
evalIntegralTrapezoid f a b eps = within eps (map snd (iterate (nextTrapezoid f a b) (1, ((f a + f b) / 2) * (b - a))))

nextSimpson :: (Double -> Double) -> Double -> Double -> (Int, Double, Double) -> (Int,  Double, Double)
nextSimpson f a b (n, trapezoid, trapezoidPrev) = (2 * n, snd (nextTrapezoid f a b (n, trapezoid)), trapezoid)

evalIntegralSimpson :: (Double -> Double) -> Double -> Double -> Double -> Either String Double
evalIntegralSimpson f a b eps = within eps (map (\(n, trapezoid, trapezoidPrev) -> (4 * trapezoid - trapezoidPrev) / 3) (iterate (nextSimpson f a b) (2, ((f a + 2 * (f ((a + b) / 2)) + f b) / 4) * (b - a), ((f a + f b) / 2) * (b - a))))
