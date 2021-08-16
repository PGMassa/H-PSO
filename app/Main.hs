module Main where

import PSO
import ToyProblems

-- | Tests the Travelling Salesman Problem
testTSP :: Population
testTSP = result
    where
        result = run fit popSize solSize solRange iterations seed

        popSize    = 100
        solSize    = fromIntegral $ length $ graph !! 0
        solRange   = (0, fromIntegral $ (length graph - 1))
        iterations = 100
        seed       = 134

        graph  = [[0,5,10,15,20],
                  [5,0,5,10,15],
                  [10,5,0,5,15],
                  [15,10,5,0,5],
                  [20,15,15,5,0]]

        simple = True -- Any and all repetition receives equal punishment
        --simple = False -- The more repetition, the bigger the punishement

        fit    = tsp graph simple

-- | Tests the Knapsack Problem
testKnapsack = result
    where 
        result = run fit popSize solSize solRange iterations seed

        popSize    = 100
        solSize    = 3
        solRange   = (0, fromIntegral $ (length itemList - 1))
        iterations = 100
        seed       = 134

        itemList  = [((7::Double) , (10::Double))
                    ,(10, 10)
                    ,(10.5, 15)
                    ,(5 , 10)
                    ,(10, 10)
                    ,(5.5 , 12.5)]
        maxWeight = 25.0 :: Double

        fit       = knapsack itemList maxWeight

main :: IO ()
main = putStrLn $ show testTSP
       --putStrLn $ show testKnapsack
