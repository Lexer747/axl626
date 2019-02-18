module Main where

import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Constraints
import Moo.GeneticAlgorithm.Random (getRandomR)

import Control.Monad (replicateM)

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
baseDate = "2016-**-**"

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
timeLimit = 300 --seconds

maxIterations :: Int
maxIterations = 100

popsize :: Int
popsize = 200

prob :: Double
prob = 0.25 --probability of the Gaussian mutation, i.e. the chance to mutate

sigma :: Double
sigma = 0.01 -- the amount to scale the mutation by

elitesize :: Int
elitesize = 5

------------- GENETIC EQUATIONS ------------

--Constrain the genome so that they can't bet more than 1 of all their money
constraints :: [Constraint Double Double]
constraints = [s .<=. 1]
    where
        s xs = sum xs

-- Our function we want to optimize, its fullG partially applied with the stocks and correlations
f :: [HPR] -> Correlations -> [Double] -> Double
f hprs cs xs = fullG extraYears baseDate cs (zip xs hprs) --see Fundamentals

--choose from the mutations with a roulette, sticking to the constraints
select :: SelectionOp Double
select = withConstraints constraints numberOfViolations Maximizing (rouletteSelect elitesize)

--See \cite{Hajime-Isao-Shigenobu}
crossover :: CrossoverOp Double
crossover = unimodalCrossoverRP

--Randomly change values with a Gaussian distribution as guidance
mutate :: MutationOp Double
mutate = gaussianMutate prob sigma

--give the 'nextGeneration' function all the parameters it needs to evaluate
--and select the most successful ones to carry their genes on
step :: [HPR] -> Correlations -> StepGA Rand Double
step hprs cs = nextGeneration Maximizing (f hprs cs) select elitesize crossover mutate

-------------- CREATE GENOMES ----------------

initGenome :: Int -> Rand [Genome Double]
initGenome i = customConstrainedGenomes popsize i
--original definition:
--initGenome i = getConstrainedGenomes (constraints i) popsize (replicate i (0,1))
--needed to be changed as the chance of actually generating a genome which fit the constraints
--would get fractorially less likely for every additional stock in the portfolio.

--init a set genomes with the constraints baked into the function
customConstrainedGenomes :: Int -> Int -> Rand [Genome Double]
customConstrainedGenomes numberOfGenomes lengthOfGenome = replicateM numberOfGenomes $ induvidualGenome lengthOfGenome

--Given the length of the genome, initalize it to random values complying with the constraint
induvidualGenome :: Int -> Rand (Genome Double)
induvidualGenome i = fixInit $ let xs = replicate i (0,1) in
                     mapM getRandomR xs --start off truly random, almost 0 chance to satisfy constraint

--Randomly reduce each value in the genome till we satisfy our constraint
fixInit :: Rand (Genome Double) -> Rand (Genome Double)
fixInit genome = do
                    g <- genome
                    if (sum g <= 1) then return g --if it satisfies then return
                                    else fixInit $ mapM takeAwayRand g

--given a number, choose a number from 0 up to that number, and take it away from the original
takeAwayRand :: Double -> Rand Double
takeAwayRand x = do
                    r <- getRandomR (0,x)
                    return $ x - r

------------------- MAIN ----------------------

logStats :: Int -> Population Double -> IO ()
logStats iterno pop = putStrLn $ (show iterno) ++ ": Pop: " ++ (show pop)

geneticAlgorithm :: Int -> [HPR] -> Correlations -> IO (Population Double)
geneticAlgorithm i hprs cs = do
    runIO (initGenome i) innerLoop
    where innerLoop = loopIO 
                    [DoEvery 1 (logStats)] --, TimeLimit timeLimit
                    (Generations maxIterations)
                    (step hprs cs)

tab :: String
tab = "    "

main :: IO ()
main = do
        (len, hprs, correlations) <- initCorrelationsAndData
        putStrLn $ "Init complete: \n" ++ tab ++ "Number of stocks: " ++ (show len) ++ "\n" ++ tab ++ "Stocks: " ++ (summariseHPRS hprs)
        finalPop <- geneticAlgorithm len hprs correlations
        let winner = takeGenome . head . bestFirst Maximizing $ finalPop
        putStrLn $ (show winner) ++ ":" ++ ""

--------------------------------------------
