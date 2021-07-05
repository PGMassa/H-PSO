module ToyProblems
    ( tsp
    ) where

import PSO
import Data.List (nub)

type Graph = [[Double]]

-- | Giving a graph and a path, returns the cost of walking the path
walkCostTSP :: Graph -> [Int] -> Double
walkCostTSP graph []  = 0
walkCostTSP graph [x] = 0
walkCostTSP graph (x:y:xs) = (cost) + (walkCostTSP graph (y:xs))
    where cost = graph !! x !! y

-- | Giving a path, returns a cost for the node repetition
repeatCostTSP :: Eq a => [a] -> Double
repeatCostTSP path = cost
    where
        repetitions = (length path) - ((length  $ nub path)-1)
        cost     
            | repetitions == 1 = 0
            | otherwise = (fromIntegral repetitions) / (fromIntegral $ length path)

tsp :: Graph -> Bool -> FitnessFunction
tsp graph simpleCost
    | simpleCost = FitnessFunction simple
    | otherwise  = FitnessFunction withRepeatCost
    where
        simple :: [Double] -> Double -- ^ punishes all repeatition equally
        simple pos
            | (length rounded) /= (length $ nub rounded) = 0
            | otherwise                                  = 1.0/(walkCostTSP graph rounded)
            where
                rounded = map round pos

        withRepeatCost :: [Double] -> Double -- ^ number of repetitions affects the punishment
        withRepeatCost pos =  (1.0/walkCost)  * (1.0 - repeatCost)
            where
                rounded    = map round pos 
                walkCost   = walkCostTSP graph rounded
                repeatCost = repeatCostTSP rounded
        









{-

-- | Just a stupid function for debugging purposes
test :: Population
test = b
    where
        fit x = 1.0 / (25.0 - (foldr (+) 0 x))
        a = PSO.FitnessFunction fit
        b = run a 50 5 (1.0, 5.0) 100 655454345-}