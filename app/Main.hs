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


main :: IO ()
main = putStrLn $ show testTSP
