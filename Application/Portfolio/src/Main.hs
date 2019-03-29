module Main (main) where

import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Random (getRandomR, getNormal, withProbability)
import Moo.GeneticAlgorithm.Multiobjective

import Control.Monad (replicateM, when)
import System.IO
import System.Environment (getArgs)
import System.Exit
import Text.Printf
import Data.List (sortBy)
import Data.HashMap.Strict (empty)

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
(genomes) <----- [ crossover ] <------- (evaluated genomes)
-}

-- Our Steps:
-- 1 Choose a time to select data from
-- 2 Calculate Correlations
-- 3 Init genetic algorithm
-- 4 run algorithm till solution

verbose :: Bool
verbose = False

toCSV :: Bool
toCSV = False

------------- CALC CORRELATIONS ------------

initCorrelationsAndData :: Integer -> String -> IO (Int, [HPR], Correlations)
initCorrelationsAndData i s = do
                            p <- checkForErrors parseAll --see CSV
                            let hprs = completeAllHPR p i s --see CSV
                            let cs = calcCorrelate hprs i s empty --see Risk
                            return $ (length hprs, hprs, cs)

------------- GENETIC CONSTANTS ------------

timeLimit :: Double
timeLimit = 3 * 60 --seconds

maxIterations :: Int
maxIterations = 5000

popsize :: Int
popsize = 100

mutateProb :: Double
mutateProb = 0.75 --probability of a mutation for each variable in the genome

crossoverProb :: Double
crossoverProb = 0.75 --the probability to crossover sections of genes in a genome

sigma :: Double
sigma = 0.01 --the highest amount a single gene can change by

elitesize :: Int
elitesize = 1

------------- GENETIC EQUATIONS ------------

--init a list of lists of potential f's
initGenome :: Int -> Rand [Genome Double]
initGenome i = customConstrainedGenomes popsize i

crossover :: CrossoverOp Double
crossover = onePointCrossover crossoverProb

--Randomly change values with a Gaussian distribution as guidance
mutate :: MutationOp Double
mutate = customMutate mutateProb

multiObjectiveProblem :: Integer -> String -> [HPR] -> Correlations -> MultiObjectiveProblem ([Double] -> Double)
multiObjectiveProblem i s hprs cs = [(Maximizing,(\xs -> decoupleG i s cs (zip xs hprs))),(Minimizing,(\xs -> decoupleR i s cs (zip xs hprs)))]

--give the 'nextGeneration' function all the parameters it needs to evaluate
--and select the most successful ones to carry their genes on
multiObjectiveStep :: Integer -> String -> [HPR] -> Correlations -> StepGA Rand Double
multiObjectiveStep i s hprs cs = stepNSGA2bt (multiObjectiveProblem i s hprs cs) crossover mutate

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
takeAwayRand x | x < 0  = error $ "testing:" ++ (show x)
takeAwayRand x | x == 0 = return x
takeAwayRand x = do
                    r <- getRandomR (0,x)
                    return $ x - r

customMutate :: Double -> MutationOp Double
customMutate p old = fixMutate old $ adaptedMutate p sigma old

singleMutate :: Double -> Double -> Rand Double
singleMutate s v = do
    n <- getNormal
    let n' = s * n
    if (v + n') < 0 then return $ v - n'
                    else return $ v + n'

adaptedMutate :: Double -> Double -> MutationOp Double
adaptedMutate p s vars = mapM m vars
    where m = withProbability p (singleMutate s)

fixMutate :: Genome Double -> Rand (Genome Double) -> Rand (Genome Double)
fixMutate old cur = do
    c <- cur
    if (sum c <= 1) 
        then return c
        else fixMutate old $ mapM (\(o,n) ->
            if (o == n) --did this gene mutate?
                then return n --no: so do nothing
                else takeAwayRand n) (zip old c) --yes: so mutate always making it smaller

------------------- MAIN ----------------------

naivef :: Int -> [Double]
naivef x = take x $ repeat $ 1 / (fromIntegral x)

findNumberOfPoints :: Integer -> String -> [HPR] -> Int
findNumberOfPoints i b hprs = sum $ map length $ selectData (checkAlmostEqYear i) hprs b

findBest :: Ord b => [(a,[b])] -> ((a,[b]),(a,[b]),(a,[b]))
findBest xs = (head sorted, sorted !! ((length sorted) `div` 2) , last sorted)
    where sorted = sortBy (\(_,(g1:r1:_)) (_,(g2:r2:_)) ->
                    case (flip compare g1 g2) of
                        EQ -> compare r1 r2
                        _ -> flip compare g1 g2) xs

logStats :: Integer -> String -> [HPR] -> Correlations -> Int -> Population Double -> IO ()
logStats i s hprs cs iterno pop = if (toCSV) then return () else do
    when (iterno == 0) (
        if verbose then putStrLn "# Generation best"
                   else putStrLn "Running (Each '.' represents a generation)")
    if verbose 
        then (let res = evalAllObjectives (multiObjectiveProblem i s hprs cs) pop in
              let ((_,bestG),(_, medianG),(_,bestR)) = findBest res in
              putStrLn $ unwords $ [(show iterno), "Highest Gain:", (show bestG),"Medain Gain/Risk:" ,(show medianG),  "Lowest Risk:", (show bestR)])
        else do
                 putStr ("." ++ (show iterno))
                 hFlush stdout

geneticAlgorithm :: Integer -> String -> Int -> [HPR] -> Correlations -> IO (Population Double)
geneticAlgorithm i s genomeLen hprs cs = do
    runIO (initGenome genomeLen) innerLoop
    where innerLoop = loopIO
                    [DoEvery 1 (logStats i s hprs cs), (TimeLimit timeLimit)]
                    (Generations maxIterations)
                    (multiObjectiveStep i s hprs cs)

tab :: String
tab = "    "

main :: IO ()
main = do
        a <- getArgs
        when (length a /= 2) $
            exitFailure
        let s = a !! 0
        when (length s /= 4) $
            exitFailure
        let i = read $ a !! 1
        (len, hprs, correlations) <- initCorrelationsAndData i s
        let details = tab ++ "\
\Year Range in use: " ++ (take 4 s) ++ " ± " ++ (show i) ++ "\n" ++ tab ++ "\
\Number of stocks: " ++ (show len) ++ "\n" ++ tab ++ "\
\Number of correlations: " ++ (show $ length correlations) ++ "\n" ++ tab ++ "\
\Time to run: " ++ (show timeLimit) ++ "s\n" ++ tab ++ "\
\Population size: " ++ (show popsize) ++ "\n" ++ tab ++ "\
\Elite Size: " ++ (show elitesize) ++ "\n" ++ tab ++ "\
\Number of data Points: " ++ (show $ findNumberOfPoints i s hprs) ++ "\n" ++ tab ++ "\
\Naive f Forward Test: " ++ (show $ forwardTest i s (zip (naivef len) hprs)) ++ "\n" ++ tab ++ "\
\Best Stock: " ++ (show $ findBestStock i s hprs) ++ "\n" ++ tab ++ "\
\Best Stock Forward Test: " ++ (show $ forwardTest i s (zip (createSpread hprs $ fst $ findBestStock i s hprs) hprs))

        finalPop <- geneticAlgorithm i s len hprs correlations
        let (bestG,medianG,bestR) = findBest $ evalAllObjectives (multiObjectiveProblem i s hprs correlations) finalPop
        when (not toCSV) $ do 
            putStrLn $ "\n\
    \Finished!\n" ++ tab ++ "\
    \Best G Forward Test: " ++ (show $ forwardTest i s (zip (fst bestG) hprs)) ++ "\n" ++ tab ++ "\
    \Medain G R Forward Test: " ++ (show $ forwardTest i s (zip (fst medianG) hprs)) ++ "\n" ++ tab ++ "\
    \Best R Forward Test: " ++ (show $ forwardTest i s (zip (fst bestR) hprs)) ++ "\n" ++ tab ++ "\
    \Best G Decoupled: " ++ (show $ snd bestG) ++ "\n" ++ tab ++ "\
    \Medain G R Decoupled: " ++ (show $ snd medianG) ++ "\n" ++ tab ++ "\
    \Best R Decoupled: " ++ (show $ snd bestR) ++ "\n" ++ tab ++ "\
    \with Best G f's: " ++ (show $ fst bestG) ++ "\n" ++ tab ++ "\
    \with Median G R f's: " ++ (show $ fst medianG) ++ "\n" ++ tab ++ "\
    \with Best R f's: " ++ (show $ fst bestR)
            putStrLn details
        when toCSV $ do
            printf 
                "%s ± %d, %.7f, %s, %.7f, %.7f, %.7f, %.7f, %.3f, %.3f, %.3f, %.3f, %.3f, %.3f" 
                (take 4 s)
                i
                (forwardTest i s (zip (naivef len) hprs))
                (name $ fst $ findBestStock i s hprs)
                (forwardTest i s (zip (createSpread hprs $ fst $ findBestStock i s hprs) hprs))
                (forwardTest i s (zip (fst bestG) hprs))
                (forwardTest i s (zip (fst medianG) hprs))
                (forwardTest i s (zip (fst bestR) hprs))
                (head $ snd bestG) (last $ snd bestG)
                (head $ snd medianG) (last $ snd medianG)
                (head $ snd bestR) (last $ snd bestR)
        {-
            foldMap (\x -> printf "%.5f\n" x) $ fst bestG
            putStrLn ""
            foldMap (\(innerG:innerR:[]) -> printf "%.7f , %.7f\n" innerG innerR) $ map snd $ evalAllObjectives (multiObjectiveProblem i s hprs correlations) finalPop-}
