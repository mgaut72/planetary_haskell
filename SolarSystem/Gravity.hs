module SolarSystem.Gravity where
import Planet

g :: Double
g = 39.4784176043574320

mSun :: Double
mSun = 1.0

circularVelocity :: Planet -> Double
circularVelocity p = sqrt $ g * mSun / radius
 where radius = orbitalRadius p

updateAcc :: Planet -> Planet
updateAcc p = p { acc = (magnitude * cos theta, magnitude * sin theta) }
  where r = orbitalRadius p
        theta = orbitalAngle p
        magnitude = -1.0*g*mSun/(r*r)
