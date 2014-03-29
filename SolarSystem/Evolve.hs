module SolarSystem.Evolve where
import Control.Monad.Writer
import Planet
import Util
import LeapFrog
import SolarSystem.Gravity

etaTimeStep :: Double
etaTimeStep = 0.0004

globalDt :: Double
globalDt = 0.5 / 365.25

calcDt :: Planet -> Double
calcDt p = etaTimeStep * min (1.0 / abs v) (1.0 / abs a)
  where v = magnitude $ vel p
        a = magnitude $ acc p

evolveSystem :: Double -> Double -> Double -> [Planet] -> Writer [String] [Planet]
evolveSystem tMax t dt ps
  | t >= tMax     = tell ["done"] >> return ps  --writer (ps, ["Done with simulation"])
  | t == 0.0      = tell ["start simulation"] >> evolveSystem tMax (t+dt) dt (startSystem ps)
  | t + dt > tMax = tell ["time step too large"] >> evolveSystem tMax t (tMax-t) ps
  | otherwise     = tell ["taking normal step"] >> evolveSystem tMax (t+dt) dt (map (evolvePlanet (t+dt)) ps)

startSystem []     = []
startSystem (p:ps) = newP : startSystem ps
  where (x0, x1) = pos p
        (v0, v1) = vel p
        (a0, a1) = acc p
        t = time p
        x0new = xFirstStep x0 v0 a0 (dt p)
        x1new = xFirstStep x1 v1 a1 (dt p)
        pIntermetiate = p { pos = (x0new, x1new)
                          , time = t + 0.5*(dt p)
                          , dt = calcDt p
                          }
        newP = updateAcc pIntermetiate

evolvePlanet tEnd p = evolveCount . evolveTime . updateAcc
                      . evolvePos . evolveVel . adjustDt tEnd $ p

-- check if this planets timestep exceeds global timestep
adjustDt tEnd p
  | time p + dt p > tEnd = p { dt = tEnd - time p }
  | otherwise            = p

evolveVel p = p { vel = (v0new, v1new) }
  where v0new = vFullStep v0 a0 (dt p)
        v1new = vFullStep v1 a1 (dt p)
        (v0, v1) = vel p
        (a0, a1) = acc p

evolvePos p = p { pos = (x0new, x1new) }
  where x0new = xFullStep x0 v0 (dt p)
        x1new = xFullStep x1 v1 (dt p)
        (x0, x1) = pos p
        (v0, v1) = vel p

evolveTime p = p { time = (time p) + (dt p) , dt = calcDt p }

evolveCount p = p { step = (step p) + 1 }
