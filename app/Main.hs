module Main where

import Integration (evalIntegralMidPoint, evalIntegralTrapezoid, evalIntegralSimpson)

main :: IO ()
main = do
  print (evalIntegralSimpson (\x -> 2 * x + 1 / sqrt (x + 1/16)) 0.0 1.5 0.00001)
