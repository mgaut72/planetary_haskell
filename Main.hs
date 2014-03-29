import Control.Monad.Writer

import SolarSystem.Evolve
import SolarSystem.Create (planets)
import Control.Monad.Writer

main = do
  (_, log) <- return $ runWriter runSystem
  putStr $ unlines $ log


runSystem = evolveSystem 1 0.0 globalDt planets
