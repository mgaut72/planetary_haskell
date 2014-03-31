import Control.Monad.Writer
import SolarSystem.Evolve
import SolarSystem.Create (planets)

main = do
  (_, log) <- return $ runWriter runSystem
  putStr $ unlines log

runSystem = evolveSystem 0 1 0.0 globalDt planets
