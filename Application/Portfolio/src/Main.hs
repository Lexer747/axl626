module Main where

import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Constraints

import Fundamentals
import CSV
import Types
import Risk

{- On life cycle of the genetic algorithm:
[ start ]
    |
    v
(genomes) --> [calculate objective] --> (evaluated genomes) --> [ stop ]
    ^  ^                                       |
    |  |                                       |
    |  `-----------.                           |
    |               \                          v
[ mutate ]        (elite) <-------------- [ select ]
    ^                                          |
    |                                          |
    |                                          |
    |                                          v
(genomes) <----- [ crossover ] <-------- (evaluted genomes)
-}


-- Our Steps:
-- 1 Choose a time to select data from
-- 2 Calculate Correlations
-- 3 Init genetic algorithm
-- 4 run algorithm till solution

------------- CSV CONSTANTS --------------

baseDate :: String
baseDate = "2017-**-**"

extraYears :: Integer
extraYears = 2

------------- CALC CORRELATIONS ------------

initCorrelationsAndData :: IO (Int, [HPR], Correlations)
initCorrelationsAndData = do
                            p <- checkForErrors parseAll --see CSV
                            let hprs = makeAllHPR p --see CSV
                            let cs = calcCorrelate hprs extraYears baseDate --see Risk
                            return $ (length hprs, hprs, cs)

naivef :: Int -> [Double]
naivef x = take x $ repeat $ 1 / (fromIntegral x)

------------- GENETIC CONSTANTS ------------

timeLimit :: Double
timeLimit = 60 --seconds

maxIterations :: Int
maxIterations = 1

popsize :: Int
popsize = 200

prob :: Double
prob = 0.25 --probability of the Gaussian mutation, i.e. the chance to mutate

sigma :: Double
sigma = 0.1 -- the amount to scale the mutation by

elitesize :: Int
elitesize = 5

------------- GENETIC EQUATIONS ------------

constraints :: Int -> [Constraint Double Double]
constraints i = [s .<=. 1, l .==. (fromIntegral i)]
    where
        s xs = sum xs
        l xs = fromIntegral $ length xs

f :: [HPR] -> Correlations -> [Double] -> Double
f hprs cs xs = fullG extraYears baseDate cs (zip xs hprs) --see Fundamentals

select :: Int -> SelectionOp Double
select i = withConstraints (constraints i) numberOfViolations Maximizing (stochasticUniversalSampling elitesize)

crossover :: CrossoverOp Double
crossover = unimodalCrossoverRP

mutate :: MutationOp Double
mutate = gaussianMutate prob sigma

step :: Int -> [HPR] -> Correlations -> StepGA Rand Double
step i hprs cs = nextGeneration Maximizing (f hprs cs) (select i) elitesize crossover mutate


initGenome :: Int -> Rand [Genome Double]
initGenome i = getConstrainedGenomes (constraints i) popsize [(0,1)]

logStats :: Int -> Population Double -> IO ()
logStats n something = putStrLn $ "hi" ++ (show n) ++ " : " ++ (show something) 

--------------------------------------------

geneticAlgorithm :: Int -> [HPR] -> Correlations -> IO (Population Double)
geneticAlgorithm i hprs cs = do
    runIO (initGenome i) innerLoop
    where innerLoop = loopIO 
                    [DoEvery 1 (logStats), TimeLimit timeLimit]
                    (Generations maxIterations)
                    (step i hprs cs)

tab :: String
tab = "    "

main :: IO ()
main = do
        (len, hprs, correlations) <- initCorrelationsAndData
        putStrLn $ "Init complete: \n" ++ tab ++ "Number of stocks: " ++ (show len) ++ "\n" ++ tab ++ "Stocks: " ++ (summariseHPRS hprs)
        finalPop <- geneticAlgorithm len hprs correlations
        let winner = takeGenome . head . bestFirst Maximizing $ finalPop
        putStrLn $ (show winner) ++ ":" ++ ""