module Util where

magnitude :: (Double, Double) -> Double
magnitude (x0, x1) = sqrt $ x0*x0 + x1*x1
