-- TODO: 
--       Implement a default version of updateCoeffs
--       Implement a real genRandomWeights, using a seed

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

        _coeffs     :: Coefficients -- ^ c1 and c2 coefficients
    } 

instance Show Population where
    show pop = "Stats:" ++ stats ++ "\n\n" ++ "Particles:\n" ++ particlesStr
        where
            stats   = bestPos ++ bestFit
            bestPos = "\n  Best Position:\t" ++ show(_g_best_pos pop)
            bestFit = "\n  BestFit:\t" ++ show(_g_best_fit pop)

            particlesList = map show (_particles pop)
            particlesStr  = foldr (++) [] particlesList

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

            maxVel       = (upperB - lowerB) / 10
            initPosition = generator (lowerB, upperB) (seed * solSize)
            initVelocity = generator (0, maxVel) (round $ sum initPosition)

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
updateCoeffs a = (0.5::Double, 0.5::Double)


-- | Computes the new position and velocity of a particle
updateParticle :: ParticleState -> Position -> Position -> Range -> Coefficients -> RndWeights -> ParticleState
updateParticle (currentPos, currentVel) cognitivePos socialPos (lowerB, upperB) (c1, c2) (w, r1, r2) = 
    (newPos, newVel)
    where
        inertia   = map (* w) (currentVel)
        cognitive = map (* (c1 * r1)) (zipWith (-) cognitivePos currentPos)
        social    = map (* (c2 * r2)) (zipWith (-) socialPos currentPos)

        unboundedPos = zipWith (+) currentPos newVel
        unboundedVel = map sum $ transpose [inertia, cognitive, social]
        newPos       = map clampPos $ unboundedPos
        newVel       = map clampVel $ unboundedVel

        maxVel                   = (upperB - lowerB) / 10
        clamp (lower, upper) pos = min upper $ max lower pos
        clampPos                 = clamp (lowerB, upperB) 
        clampVel                 = clamp (0, maxVel)


-- | Behavior of a single particle during an iterations 
iterateParticle :: Coefficients -> FitnessFunction -> Position -> Range -> Particle -> Particle
iterateParticle coeffs (FitnessFunction fit) globalBest solRange particle = 
    Particle 
        { _position   = newPos
        , _velocity   = newVel
        , _p_best_fit = newBestFit
        , _p_best_pos = newBestPos
        }
        where
            rndWeights = genRandomWeights
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
iteratePopulation :: FitnessFunction -> Range -> Population -> Population
iteratePopulation fit solRange currentPop = 
    Population 
        { _particles  = newPop
        , _g_best_fit = newBestFit
        , _g_best_pos = newBestPos
        , _coeffs     = newCoeffs   
        }
        where
            iterator   = iterateParticle (_coeffs currentPop) fit (_g_best_pos currentPop) solRange
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
        initCoeffs     = (1.0::Double, 0.0::Double)
        initPopulation = genRandomPopulation popSize solSize solRange initCoeffs fit seed
        iterator       = iteratePopulation fit solRange

-- | Just a stupid function for debugging purposes
test :: Population
test = b
    where
        fit x = 1.0 / (25.0 - (foldr (+) 0 x))
        a = FitnessFunction fit
        b = run a 50 5 (1.0, 5.0) 100 655454345


someFunc :: IO ()
someFunc = putStrLn "someFunc"






-- PARA USAR NUMEROS ALEATORIOS SIGA AS SEGUINTES INSTRUÇÕES:
-- g <- getStdGen   (gera o gerador de numerso aleatorios)
-- (i, g') = randomR (0, 1.0 :: Double) g (gera numero aleatorio entre 0 e 1 e um novo gerador)