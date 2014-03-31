module LeapFrog where

xFirstStep :: Double -> Double -> Double -> Double -> Double
xFirstStep x v a dt = x + 0.5*v*dt + 0.25*a*dt*dt

xHalfStep :: Double -> Double -> Double -> Double
xHalfStep x v dt = x + 0.5*v*dt

xFullStep :: Double -> Double -> Double -> Double
xFullStep x v dt = x + v*dt

vFullStep :: Double -> Double -> Double -> Double
vFullStep v a dt = v + a*dt
