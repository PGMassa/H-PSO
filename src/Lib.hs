-- TODO: Implement function iterate population
--       Generate the coefficients w r1 and r2 randomly
--       limpar e melhorar codigo
--       Test it
--       Move on to the next functions!!!

module Lib
    ( someFunc
    ) where

import Data.List (transpose)
import System.Random

type Position       = [Double]
type Velocity       = [Double]
type ParticleState  = (Position, Velocity)
type Coefficients   = (Double, Double)
type RndWeights     = (Double, Double, Double)

-- | Stores function for fitness calculation 
data FitnessFunction = FitnessFunction (Position -> Double)

-- | Stores information about individual particle
data Particle = Particle 
    {
        _position   :: Position, -- ^ current position of the particle
        _velocity   :: Velocity, -- ^ current velocity of the particle

        _p_best_fit :: Double, -- ^ personal best fitness
        _p_best_pos :: Position -- ^ position where this particle found its best fitness
    } deriving (Show)

-- | Stores information about the population of particles
data Population = Population
    {
        _particles  :: [Particle], -- ^ list containing whole population

        _g_best_fit :: Double, -- ^ global best fitness
        _g_best_pos :: Position, -- ^ position where this population found its best fit

        _coeffs     :: Coefficients -- ^ c1 and c2 coefficients
    } deriving (Show)

-- | RESOLVER ESSA FUNCAO DEPOIS
-- | Generates w, r1 and r2
genRandomWeights :: (Double, Double, Double)
genRandomWeights = (1.0, 1.0, 1.0)

-- | Computes the new position and velocity of a particle
updateParticle :: ParticleState -> Position -> Position -> Coefficients -> RndWeights -> ParticleState
updateParticle (currentPos, currentVel) cognitivePos socialPosition (c1, c2) (w, r1, r2) = 
    (newPos, newVel)
    where
        inertia   = map (* w) (currentVel)
        cognitive = map (* (c1 * r1)) (zipWith (-) cognitivePos currentPos)
        social    = map (* (c2 * r2)) (zipWith (-) socialPosition currentPos)

        newPos    = zipWith (+) currentPos newVel
        newVel    = map sum $ transpose [inertia, cognitive, social]

-- | Behavior of a single particle during an iterations 
iterateParticle :: Particle -> Coefficients -> FitnessFunction -> Position -> Particle
iterateParticle particle coeffs (FitnessFunction fit) globalBest = 
    Particle 
        { _position   = newPos
        , _velocity   = newVel
        , _p_best_fit = newBestFit
        , _p_best_pos = newBestPos
        }
        where
            rndWeights = genRandomWeights
            current_state    = ((_position particle), (_velocity particle))
            (newPos, newVel) = updateParticle current_state (_p_best_pos particle) globalBest coeffs rndWeights

            newFit     = fit newPos
            (newBestFit, newBestPos)
                | newFit > _p_best_fit particle = (newFit, newPos)
                | otherwise                     = (_p_best_fit particle, _position particle)

iteratePopulation :: Population -> Population
iteratePopulation currentPop = 
    Population 
        { _particles  = newPop
        , _g_best_fit = newBestFit
        , _g_best_pos = newBestPos
        , _coeffs     = newCoeffs   
        }
        where
            newPop     = undefined
            newBestFit = undefined
            newBestPos = undefined
            newCoeffs  = undefined

someFunc :: IO ()
someFunc = putStrLn "someFunc"






-- PARA USAR NUMEROS ALEATORIOS SIGA AS SEGUINTES INSTRUÇÕES:
-- g <- getStdGen   (gera o gerador de numerso aleatorios)
-- (i, g') = randomR (0, 1.0 :: Double) g (gera numero aleatorio entre 0 e 1 e um novo gerador)