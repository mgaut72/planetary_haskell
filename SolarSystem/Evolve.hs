module SolarSystem.Evolve where
import Control.Monad.Writer
import Planet
import Util
import LeapFrog
import SolarSystem.Gravity
import Text.Printf

etaTimeStep :: Double
etaTimeStep = 0.0004

globalDt :: Double
globalDt = 0.5 / 365.25

calcDt :: Planet -> Double
calcDt p = etaTimeStep * min (1.0 / abs v) (1.0 / sqrt (abs a))
  where v = magnitude $ vel p
        a = magnitude $ acc p

evolveSystem :: Int -> Double -> Double -> Double -> [Planet] -> Writer [String] [Planet]
evolveSystem step tMax t dt ps
  | t >= tMax     = logSystem step t dt ps >> return ps
  | t + dt > tMax = evolveSystem step tMax t (tMax-t) ps
  | otherwise     = logSystem step t dt ps >> nextRound evolved
 where evolved = map (evolvePlanet (t+dt)) ps
       nextRound = evolveSystem (step+1) tMax (t+dt) dt


logSystem step time delta ps = globalLog >> logPlanets ps
  where globalLog = tell [ printf "%d\t" step ++ printf "%.6f\t" time
                        ++ printf "%.6f\t" delta ]

firstStep p = newP
  where (x0, x1) = pos p
        (v0, v1) = vel p
        (a0, a1) = acc p
        x0new = xFirstStep x0 v0 a0 (dt p)
        x1new = xFirstStep x1 v1 a1 (dt p)
        pIntermetiate = p { pos = (x0new, x1new) }
        newP = (\p -> p {time = time p + 0.5 * dt p })
             . (\p -> p { dt = calcDt p})
             . updateAcc $ pIntermetiate

evolvePlanet tEnd p
  | time p == 0.0 = evolvePlanet tEnd $ firstStep p
  | time p < tEnd = evolvePlanet tEnd
                    ( evolveCount . evolveTime . updateAcc
                    . evolvePos . evolveVel . adjustDt tEnd $ p)
  | otherwise = p

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

evolveTime p = p { time = time p + dt p , dt = calcDt p }

evolveCount p = p { step = step p + 1 }
