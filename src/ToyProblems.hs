module ToyProblems
    ( tsp, knapsack
    ) where

import PSO
import Data.List (nub)

type Graph = [[Double]]

-- | Given a graph and a path, returns the cost of walking the path
walkCostTSP :: Graph -> [Int] -> Double
walkCostTSP graph []  = 0
walkCostTSP graph [x] = 0
walkCostTSP graph (x:y:xs) = (cost) + (walkCostTSP graph (y:xs))
    where cost = graph !! x !! y

-- | Given a path, returns a cost for the node repetition
repeatCostTSP :: Eq a => [a] -> Double
repeatCostTSP path = cost
    where
        repetitions = (length path) - ((length  $ nub path)-1)
        cost     
            | repetitions == 1 = 0
            | otherwise = (fromIntegral repetitions) / (fromIntegral $ length path)

-- | Given a graph, returns a Fitness Function for the TSP problem
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

-- | Given a List of (weight, value), returns a Fitness Function for the Knapsack Problem
knapsack :: [(Double, Double)] -> Double -> FitnessFunction
knapsack items maxWeight = FitnessFunction knapsackValue
    where 
        knapsackValue :: [Double] -> Double
        knapsackValue packed 
            | overweightKnapsack = 0.0
            | otherwise          = sumValueItems 
                where 
                    packedIndex = map round packed
                    packedItems = unzip $ [items !! index | index <- packedIndex]

                    overweightKnapsack = (sum $ fst packedItems) > maxWeight
                    sumValueItems      = sum $ snd packedItems
            

        





