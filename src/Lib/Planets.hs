-- https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html
module Lib.Planets where

import Control.Monad
import System.Environment   

data Planet = Mercury | Venus | Earth | Mars | Jupiter | Saturn | Uranus | Neptune | Pluto deriving (Enum, Show, Eq)

data PlanetStat = PlanetStat { planet :: Planet, mass :: Double, radius :: Double }

stat :: Planet -> PlanetStat
stat Mercury = PlanetStat { planet = Mercury, mass = 3.303e+23, radius = 2.4397e6  }
stat Venus   = PlanetStat { planet = Venus,   mass = 4.869e+24, radius = 6.0518e6  }
stat Earth   = PlanetStat { planet = Earth,   mass = 5.976e+24, radius = 6.37814e6 }
stat Mars    = PlanetStat { planet = Mars,    mass = 6.421e+23, radius = 3.3972e6  }
stat Jupiter = PlanetStat { planet = Jupiter, mass = 1.9e+27,   radius = 7.1492e7  }
stat Saturn  = PlanetStat { planet = Saturn,  mass = 5.688e+26, radius = 6.0268e7  }
stat Uranus  = PlanetStat { planet = Uranus,  mass = 8.686e+25, radius = 2.5559e7  }
stat Neptune = PlanetStat { planet = Neptune, mass = 1.024e+26, radius = 2.4746e7  }
stat Pluto   = PlanetStat { planet = Pluto,   mass = 1.27e+22,  radius = 1.137e6   }

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

