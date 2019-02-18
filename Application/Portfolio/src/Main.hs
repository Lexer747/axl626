module Main where

import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Random (getRandomR, getNormal, withProbability)

import Control.Monad (replicateM, when)

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
popsize = 100

prob :: Double
prob = 0.1 --probability of a mutation for each variable in the genome

sigma :: Double
sigma = 0.1 --the highest amount a single gene can change by

elitesize :: Int
elitesize = 5

------------- GENETIC EQUATIONS ------------


-- Our function we want to optimize, its fullG partially applied with the stocks and correlations
f :: [HPR] -> Correlations -> [Double] -> Double
f hprs cs xs = fullG extraYears baseDate cs (zip xs hprs) --see Fundamentals

--init a list of lists of potential f's
initGenome :: Int -> Rand [Genome Double]
initGenome i = customConstrainedGenomes popsize i

--choose from the mutations with a roulette, sticking to the constraints
select :: SelectionOp Double
select = rouletteSelect elitesize

--See \cite{Hajime-Isao-Shigenobu}
crossover :: CrossoverOp Double
crossover = unimodalCrossoverRP

--Randomly change values with a Gaussian distribution as guidance
mutate :: MutationOp Double
mutate = customMutate prob

--give the 'nextGeneration' function all the parameters it needs to evaluate
--and select the most successful ones to carry their genes on
step :: [HPR] -> Correlations -> StepGA Rand Double
step hprs cs = nextGeneration Maximizing (f hprs cs) select elitesize crossover mutate

-------------- CREATE GENOMES ----------------

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
takeAwayRand x | x <= 0 = return x
takeAwayRand x = do
                    r <- getRandomR (0,x)
                    return $ x - r

customMutate :: Double -> MutationOp Double
customMutate p old = fixMutate old $ adaptedMutate p sigma old

adaptedMutate :: Double -> Double -> MutationOp Double
adaptedMutate p s vars = mapM m vars
    where m = withProbability p $ \v -> do
               n <- getNormal
               return $ if (v + s*n) < 0 then (v - s*n) --check for negatives
                                         else (v + s*n)

fixMutate :: Genome Double -> Rand (Genome Double) -> Rand (Genome Double)
fixMutate old cur = do
    c <- cur
    if (sum c <= 1) then return c
                    else fixMutate old $ mapM (\(o,n) -> if (o == n)
                            then return n
                            else takeAwayRand n) (zip old c)

------------------- MAIN ----------------------

verfiy :: [Double] -> Bool
verfiy xs = sum xs <= 1 && sum xs >= 0

logStats :: Int -> Population Double -> IO ()
logStats iterno pop = do
    when (iterno == 0) $
        putStrLn "# Generation best median worst genomeOfBest"
    let gs = bestFirst Maximizing $ pop
    let (bestG,best) = head gs
    let (_,median) = gs !! (length gs `div` 2)
    let (_,worst) = last gs
    putStrLn $ unwords [(show iterno), (take 5 $ show best), (take 5 $ show median), (take 5 $ show worst), (show $ verfiy bestG), (succinctList bestG)]

geneticAlgorithm :: Int -> [HPR] -> Correlations -> IO (Population Double)
geneticAlgorithm i hprs cs = do
    runIO (initGenome i) innerLoop
    where innerLoop = loopIO 
                    [DoEvery 1 (logStats), (TimeLimit timeLimit)] --
                    (Generations maxIterations)
                    (step hprs cs)

tab :: String
tab = "    "

main :: IO ()
main = do
        (len, hprs, correlations) <- initCorrelationsAndData
        putStrLn $ "Init complete: \n" ++ tab ++ "Number of stocks: " ++ (show len) ++ "\n" ++ tab ++ "Number of correlations: " ++ (show $ length correlations) ++ "\n"
        finalPop <- geneticAlgorithm len hprs correlations
        let winner = takeGenome . head . bestFirst Maximizing $ finalPop
        putStrLn $ (show winner) ++ ":" ++ ""

--------------------------------------------
