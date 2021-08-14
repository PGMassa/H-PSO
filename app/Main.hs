-- TODO: 
--       Test the TSP
--       Implemente one or two more toy problems
--       Refactore and comment the PSO module
--       Refactore and comment the ToyProblems module
--       Refactore and comment the Main module
--       Implement some sort of visualization and a way to store the results
--       Try a harder problem

module Main where

import PSO
import ToyProblems

-- | Tests the Travelling Salesman Problem
testTSP :: Population
testTSP = result
    where
        result = run fit popSize solSize solRange iterations seed

        popSize    = 50
        solSize    = fromIntegral $ length $ graph !! 0
        solRange   = (0, fromIntegral $ (length graph - 1))
        iterations = 50
        seed       = 134

        graph  = [[0,2,1],
                  [2,0,2],
                  [2,1,0]]

        --simple = True -- Any and all repetition receives equal punishment
        simple = False -- The more repetition, the bigger the punishement

        fit    = tsp graph simple

-- | Tests the Knapsack Problem
testKnapsack = result
    where 
        result = run fit popSize solSize solRange iterations seed

        popSize    = 50
        solSize    = 2
        solRange   = (0, fromIntegral $ (length itemList - 1))
        iterations = 50
        seed       = 134

        itemList  = [((5::Double) , (100::Double))
                    ,(10, 100)
                    ,(10, 20)
                    ,(5 , 20)
                    ,(10, 10)
                    ,(5 , 10)] 
        maxWeight = 15.0 :: Double

        fit      = knapsack itemList maxWeight

main :: IO ()
main = --putStrLn $ show testTSP
       putStrLn $ show testKnapsack
