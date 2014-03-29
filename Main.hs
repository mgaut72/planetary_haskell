import Control.Monad.Writer

import SolarSystem.Evolve
import SolarSystem.Create (planets)
import Control.Monad.Writer

--main = do
--  out <- runSystem
--  putStrLn $ snd $ runWriter out

runSystem = evolveSystem 1 0.0 globalDt planets
