{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Test.Integration where

import Test.Tasty.HUnit (Assertion, assertBool)
import Integration (evalIntegralMidPoint)

is_within :: Either String Double -> Double -> Double -> Bool
is_within (Left _) _ _ = False
is_within (Right a) b eps = abs (a - b) < eps

unit_evalIntegralMidPoint :: Assertion
unit_evalIntegralMidPoint = do
    assertBool "wrong answer on case 1" (is_within (evalIntegralMidPoint (\x -> 2 * x + 1 / sqrt (x + 1/16)) 0.0 1.5 1e-5) 4.25 1e-5)