module SolarSystem.Create where
import Planet
import SolarSystem.Gravity
import SolarSystem.Evolve

auInKm :: Double
auInKm = 1.495979e8

createPlanet :: Double -> Double -> Planet
createPlanet a e = initDt . updateAcc . initVelocity $ p
  where p = Planet { pos = (a * (1 - e), 0.0)
                   , vel = (undefined, undefined)
                   , acc = (undefined, undefined)
                   , time = 0.0
                   , dt = undefined
                   , eccentricity = e
                   , sma = a
                   , step = 0
                   }
        initVelocity p = p { vel = (0.0,  circularVelocity p * sqrt (1 + e)) }
        initDt p = p { dt = calcDt p }

mercury = createPlanet (57909227.0 / auInKm) 0.20563593

venus = createPlanet (108209475.0 / auInKm) 0.00677672

earth = createPlanet 1.0 0.01671123

planets = [mercury, venus, earth]
