module Planet where
import Util
import Control.Monad.Writer

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

logDriftedPlanet :: Planet -> Writer [String] Planet
logDriftedPlanet p = writer (p, [state])
  where stp = show $ step p
        t = time p
        dt_ = dt p
        (x0, x1) = pos p
        (v0, v1) = vel p
        (a0, a1) = acc p
        v0drift = v0 + 0.5*a0*dt_
        v1drift = v1 + 0.5*a1*dt_
        state = show t ++ "\t" ++ show dt_ ++ "\t" ++ show x0 ++ "\t"
             ++ show x1 ++ "\t" ++ show v0drift ++ "\t" ++ show v1drift ++ "\t"
             ++ show a0 ++ "\t" ++ show a1 ++ "\t"

logPlanets :: [Planet] -> Writer [String] [Planet]
logPlanets = mapM logDriftedPlanet
