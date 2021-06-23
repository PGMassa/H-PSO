module Lib
    ( someFunc
    ) where

-- | Stores information about individual particle
data Particle = Particle 
    {
        _position   :: [Double], -- ^ current position of the particle
        _velocity   :: [Double], -- ^ current velocity of the particle

        _p_best_fit :: Double, -- ^ personal best fitness
        _p_best_pos :: [Double] -- ^ position where this particle found its best fitness
    } deriving (Show)

-- | Stores information about the population of particles
data Population = Population
    {
        _particles  :: [Particle], -- ^ list containing whole population

        _g_best_fit :: Double, -- ^ global best fitness
        _g_best_pos :: [Double] -- ^ position where this population found its best fit
    } deriving (Show)

-- | Stores function for fitness calculation 
data FitnessFunction = FitnessFunction ([Double] -> Double)

-- | Applies the fitness function to a single particle and returns the value
particleFitness :: FitnessFunction -> Particle -> Double
particleFitness (FitnessFunction f) p = f (_position p)


someFunc :: IO ()
someFunc = putStrLn "someFunc"
