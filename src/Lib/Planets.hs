-- https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html
module Lib.Planets where

import Control.Monad
import System.Environment   

data Planet = Mercury | Venus | Earth | Mars | Jupiter | Saturn | Uranus | Neptune deriving (Enum, Show, Eq)

data PlanetStat = PlanetStat { planet :: Planet, mass :: Double, radius :: Double }

stat :: Planet -> PlanetStat
stat p@Mercury = PlanetStat { planet = p, mass = 3.301e+23, radius = 2.4394e6 }
stat p@Venus   = PlanetStat { planet = p, mass = 4.867e+24, radius = 6.052e6  }
stat p@Earth   = PlanetStat { planet = p, mass = 5.972e+24, radius = 6.3710e6 }
stat p@Mars    = PlanetStat { planet = p, mass = 6.417e+23, radius = 3.3895e6 }
stat p@Jupiter = PlanetStat { planet = p, mass = 1.898e+27, radius = 6.9911e7 }
stat p@Saturn  = PlanetStat { planet = p, mass = 5.685e+26, radius = 5.8232e7 }
stat p@Uranus  = PlanetStat { planet = p, mass = 8.682e+25, radius = 2.362e7  }
stat p@Neptune = PlanetStat { planet = p, mass = 1.024e+26, radius = 2.4622e7 }

g :: Double
g = 6.67300E-11

calculateSurfaceGravity :: PlanetStat -> Double
calculateSurfaceGravity p = (g * mass p) / (radius p * radius p)

surfaceWeight :: Double -> PlanetStat -> Double
surfaceWeight otherMass p = otherMass * calculateSurfaceGravity p

printOutAllWeights :: Double -> IO ()
printOutAllWeights earthWeight = forM_ [toEnum 0..] message
    where
        mass = earthWeight / calculateSurfaceGravity (stat Earth)
        message planet = putStrLn $ "Your weight on " <> show planet <> " is " <> show (surfaceWeight mass (stat planet))

main :: IO ()
main = do
    args <- getArgs
    let usage = "Usage: planets <earth_weight>"
    case args of
        [first] -> printOutAllWeights (read first :: Double)
        _ -> error $ "Wrong number of arguments: " <> show args <> "\n" <> usage

