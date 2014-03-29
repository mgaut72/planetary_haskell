module Planet where
import Util

type Position = (Double, Double)
type Velocity = (Double, Double)
type Acceleration = (Double, Double)

data Planet = Planet { pos :: Position         -- AU
                     , vel :: Velocity         -- AU/yr
                     , acc :: Acceleration     -- AU/yr/yr
                     , time :: Double          -- current time in years
                     , dt :: Double            -- timestep in yr
                     , eccentricity :: Double  -- eccentricity of orbit
                     , sma :: Double           -- semi-major axis
                     , step :: Int             -- number of steps simulated
                     } deriving (Show)

orbitalRadius :: Planet -> Double
orbitalRadius p = magnitude $ pos p

orbitalAngle :: Planet -> Double
orbitalAngle p
  | x == 0.0 && y >  0.0 = 0.5 * pi
  | x == 0.0 && y <= 0.0 = 1.5 * pi
  | otherwise            = atan2 y x
 where (x,y) = pos p
