-- TODO: 
--       Implement a default version of updateCoeffs
--       Implement a real genRandomWeights, using a seed

--       Implement the main loop
--       Implement show instances for debugging purposes
--       Refactoring and commenting

--       Test every function, one at a time
--       No, no, no, I'm serious, go back there and don't come back until it's all tested
--       DON'T TRY TO FOOL ME!!! I AM YOU, I KNOW YOU DIND'T TEST SHIT!!! GO BACK THERE AND TEST IT!!!
--       Ok, good, it wasn't that hard was it :)

--       Test it a one or two toy problems
--       Implement some sort of visualization and a way to store the results
--       Try a harder problem

module Lib
    ( someFunc
    ) where

import Data.List (transpose)
import System.Random

type Position       = [Double]
type Velocity       = [Double]
type ParticleState  = (Position, Velocity)
type Coefficients   = (Double, Double) -- ^ c1 and c2
type RndWeights     = (Double, Double, Double) -- ^ w, r1 and r2
type Range          = (Double, Double) -- ^ lower and upper boundary for parameters of the solution 

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
genRandomWeights :: RndWeights
genRandomWeights = (1.0, 1.0, 1.0)

-- | Generates a list of random values between [0..1]
randomList :: Int -> Int -> [Double]
randomList size seed = take size $ randoms (mkStdGen seed) :: [Double]

-- | Generates a random vector
genRandomVector :: Int -> Range -> Int -> [Double]
genRandomVector size (lowerB, upperB) seed = vecInRange
    where
        rangeLen    = upperB - lowerB
        putInRage x = (x * rangeLen) + lowerB

        rndVector   = randomList size seed

        vecInRange  = map (putInRage) rndVector

-- | Generates a random solution
genRandomSolution :: Int -> Range -> FitnessFunction -> Int -> Particle
genRandomSolution solSize (lowerB, upperB) (FitnessFunction fit) seed =
    Particle 
        { _position   = initPosition
        , _velocity   = initVelocity
        , _p_best_fit = initBestFit
        , _p_best_pos = initBestPos
        }
        where
            generator    = genRandomVector solSize

            velRange     = (upperB - lowerB) / 10
            initPosition = generator (lowerB, upperB) (seed * solSize)
            initVelocity = generator (0, velRange) (round $ sum initPosition)

            initBestFit  = fit initPosition
            initBestPos  = initPosition

-- | Generates a random population
genRandomPopulation :: Int -> Int -> Range -> Coefficients -> FitnessFunction -> Int -> Population
genRandomPopulation popSize solSize solRange coeffs fit seed = 
    Population
        { _particles  = initParticles
        , _g_best_fit = initBestFit
        , _g_best_pos = initBestPos
        , _coeffs     = initCoeffs
        }
        where
            generator      = genRandomSolution solSize solRange fit
            initParticles  = [ generator $ x * seed | x <- [1 .. popSize]]

            initCoeffs     = coeffs

            (initBestFit, initBestPos) = bestFit initParticles

-- | EVENTUALMENTE PERMITIR QUE O USUARIO DEFINA ISSO
-- | Receives the coeffs in the current iteration and returns the coeffs for the next iteration
updateCoeffs :: Coefficients -> Coefficients
updateCoeffs = undefined


-- | Computes the new position and velocity of a particle
updateParticle :: ParticleState -> Position -> Position -> Coefficients -> RndWeights -> ParticleState
updateParticle (currentPos, currentVel) cognitivePos socialPos (c1, c2) (w, r1, r2) = 
    (newPos, newVel)
    where
        inertia   = map (* w) (currentVel)
        cognitive = map (* (c1 * r1)) (zipWith (-) cognitivePos currentPos)
        social    = map (* (c2 * r2)) (zipWith (-) socialPos currentPos)

        newPos    = zipWith (+) currentPos newVel
        newVel    = map sum $ transpose [inertia, cognitive, social]

-- | Behavior of a single particle during an iterations 
iterateParticle :: Coefficients -> FitnessFunction -> Position -> Particle -> Particle
iterateParticle coeffs (FitnessFunction fit) globalBest particle = 
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


-- | Finds the best fit in a list of particles
bestFit :: [Particle] -> (Double, Position)
bestFit population = maximum fitList 
    where
        fitList = [(_p_best_fit particle, _p_best_pos particle) | particle <- population]

-- | Behavior of the whole population during a single iteration
iteratePopulation :: Population -> FitnessFunction -> Population
iteratePopulation currentPop fit = 
    Population 
        { _particles  = newPop
        , _g_best_fit = newBestFit
        , _g_best_pos = newBestPos
        , _coeffs     = newCoeffs   
        }
        where
            iterator   = iterateParticle (_coeffs currentPop) fit (_g_best_pos currentPop)
            newPop     = map (iterator) (_particles currentPop)

            newCoeffs  = updateCoeffs (_coeffs currentPop)

            (itBestFit, itBestPos) = bestFit newPop
            (newBestFit, newBestPos) 
                | itBestFit > (_g_best_fit currentPop) = (itBestFit, itBestPos)
                | otherwise = ((_g_best_fit currentPop), (_g_best_pos currentPop))




someFunc :: IO ()
someFunc = putStrLn "someFunc"






-- PARA USAR NUMEROS ALEATORIOS SIGA AS SEGUINTES INSTRUÇÕES:
-- g <- getStdGen   (gera o gerador de numerso aleatorios)
-- (i, g') = randomR (0, 1.0 :: Double) g (gera numero aleatorio entre 0 e 1 e um novo gerador)