module PSO
    ( run, FitnessFunction (FitnessFunction), Population, Particle
    ) where

import Data.List (transpose)
import System.Random

type Position       = [Double]
type Velocity       = [Double]
type ParticleState  = (Position, Velocity)
type Coefficients   = (Double, Double, Double) -- ^ c1, c2 and step
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
    }

instance Show Particle where
    show particle = "{" ++ position ++ velocity ++ bestPos ++ bestFit ++ "\n}\n"
        where 
            position = "\n  Position:\t\t" ++ show (_position particle) 
            velocity = "\n  Velocity:\t\t" ++ show (_velocity particle)
            bestPos  = "\n  Best Position:\t" ++ show (_p_best_pos particle)
            bestFit  = "\n  Best fit:\t\t" ++ show (_p_best_fit particle)

-- | Stores information about the population of particles
data Population = Population
    {
        _particles  :: [Particle], -- ^ list containing whole population

        _g_best_fit :: Double, -- ^ global best fitness
        _g_best_pos :: Position, -- ^ position where this population found its best fit

        _coeffs     :: Coefficients -- ^ c1, c2 and step
    } 

instance Show Population where
    show pop = "Stats:" ++ stats ++ "\n\n" ++ "Particles:\n" ++ particlesStr
        where
            stats   = bestPos ++ bestFit
            bestPos = "\n  Best Position:\t" ++ show(_g_best_pos pop)
            bestFit = "\n  BestFit:\t" ++ show(_g_best_fit pop)

            particlesList = map show (_particles pop)
            particlesStr  = foldr (++) [] particlesList

-- | Generates w, r1 and r2
genRandomWeights :: Int -> RndWeights
genRandomWeights seed = (w, r1, r2)
    where
        w       = weights !! 0
        r1      = 2 * weights !! 0
        r2      = 2 * weights !! 0
        weights = randomList 3 seed

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

            maxVel       = (upperB - lowerB) / 10.0
            initPosition = generator (lowerB, upperB) (seed * solSize)
            initVelocity = generator (-maxVel, maxVel) (round $ sum initPosition)

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

-- | Updates de c1 and c2 for the next iteration
updateCoeffs :: Coefficients -> Coefficients
updateCoeffs (c1, c2, step) = (nC1, nC2, step)
    where 
        nC1 = c1 - step
        nC2 = 1.0 - nC1

-- | Computes the new position and velocity of a particle
updateParticle :: ParticleState -> Position -> Position -> Range -> Coefficients -> RndWeights -> ParticleState
updateParticle (currentPos, currentVel) cognitivePos socialPos (lowerB, upperB) (c1, c2, step) (w, r1, r2) = 
    (newPos, newVel)
    where
        inertia   = map (* w) (currentVel)
        cognitive = map (* (c1 * r1)) (zipWith (-) cognitivePos currentPos)
        social    = map (* (c2 * r2)) (zipWith (-) socialPos currentPos)

        unboundedPos = zipWith (+) currentPos newVel
        unboundedVel = map sum $ transpose [inertia, cognitive, social]
        newPos       = map clampPos $ unboundedPos
        newVel       = map clampVel $ unboundedVel

        maxVel                   = (upperB - lowerB) / 10.0
        clamp (lower, upper) pos = min upper $ max lower pos
        clampPos                 = clamp (lowerB, upperB) 
        clampVel                 = clamp (-maxVel, maxVel)


-- | Behavior of a single particle during an iterations 
iterateParticle :: Coefficients -> FitnessFunction -> Position -> Range -> Int -> Particle -> Particle
iterateParticle coeffs (FitnessFunction fit) globalBest solRange seed particle = 
    Particle 
        { _position   = newPos
        , _velocity   = newVel
        , _p_best_fit = newBestFit
        , _p_best_pos = newBestPos
        }
        where
            rndWeights = genRandomWeights seed
            current_state    = ((_position particle), (_velocity particle))
            (newPos, newVel) = updateParticle current_state (_p_best_pos particle) globalBest solRange coeffs rndWeights

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
iteratePopulation :: FitnessFunction -> Range -> Int -> Population -> Population
iteratePopulation fit solRange seed currentPop = 
    Population 
        { _particles  = newPop
        , _g_best_fit = newBestFit
        , _g_best_pos = newBestPos
        , _coeffs     = newCoeffs   
        }
        where
            iterator   = iterateParticle (_coeffs currentPop) fit (_g_best_pos currentPop) solRange seed
            newPop     = map (iterator) (_particles currentPop)

            newCoeffs  = updateCoeffs (_coeffs currentPop)

            (itBestFit, itBestPos) = bestFit newPop
            (newBestFit, newBestPos) 
                | itBestFit > (_g_best_fit currentPop) = (itBestFit, itBestPos)
                | otherwise = ((_g_best_fit currentPop), (_g_best_pos currentPop))

-- | Main loop of the PSO :)
run :: FitnessFunction -> Int -> Int -> Range -> Int -> Int -> Population
run fit popSize solSize solRange iterations seed = iterate iterator initPopulation !! iterations
    where
        c1             = 1.0 :: Double
        c2             = 1.0 - c1
        step           = (1.0 - c2)/(fromIntegral iterations)
        initCoeffs     = (c1, c2, step)

        initPopulation = genRandomPopulation popSize solSize solRange initCoeffs fit seed
        iterator       = iteratePopulation fit solRange seed
