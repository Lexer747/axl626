module Main where

import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Random (getRandomR, getNormal, withProbability)

import Control.Monad (replicateM, when)
import System.IO

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

verbose :: Bool
verbose = False

------------- CSV CONSTANTS --------------

baseDate :: String
baseDate = "2014-**-**"

extraYears :: Integer
extraYears = 1

------------- CALC CORRELATIONS ------------

initCorrelationsAndData :: IO (Int, [HPR], Correlations)
initCorrelationsAndData = do
                            p <- checkForErrors parseAll --see CSV
                            let hprs = makeAllHPR p --see CSV
                            let cs = calcCorrelate hprs extraYears baseDate --see Risk
                            return $ (length hprs, hprs, cs)

------------- GENETIC CONSTANTS ------------

timeLimit :: Double
timeLimit = 60 --seconds

maxIterations :: Int
maxIterations = 5000

popsize :: Int
popsize = 500

mutateProb :: Double
mutateProb = 0.1 --probability of a mutation for each variable in the genome

crossoverProb :: Double
crossoverProb = 0.5 --the probability to crossover sections of genes in a genome

sigma :: Double
sigma = 0.0001 --the highest amount a single gene can change by

elitesize :: Int
elitesize = 5

------------- GENETIC EQUATIONS ------------

-- Our function we want to optimize, its fullG partially applied with the stocks and correlations
f :: [HPR] -> Correlations -> [Double] -> Double
f hprs cs xs = if isNaN g then error $ "NaN caused by: " ++ (show xs)
                          else g
    where g = fullG extraYears baseDate cs (zip xs hprs) --see Fundamentals

--init a list of lists of potential f's
initGenome :: Int -> Rand [Genome Double]
initGenome i = customConstrainedGenomes popsize i

--choose from the mutations with a roulette, sticking to the constraints
select :: SelectionOp Double
select = rouletteSelect elitesize

crossover :: CrossoverOp Double
crossover = onePointCrossover crossoverProb

--Randomly change values with a Gaussian distribution as guidance
mutate :: MutationOp Double
mutate = customMutate mutateProb

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
    if (sum c <= 1) then return c
                    else fixMutate old $ mapM (\(o,n) -> if (o == n)
                            then return n
                            else takeAwayRand n) (zip old c)

------------------- MAIN ----------------------

naivef :: Int -> [Double]
naivef x = take x $ repeat $ 1 / (fromIntegral x)

verfiy :: [Double] -> Bool
verfiy xs = (sum xs <= 1) && (sum xs >= 0) && (foldr (&&) True (map ((<) 0) xs))
-- (show $ foldr (&&) True $ map (\ (a,_) -> verfiy a) gs)

findNumberOfPoints :: Integer -> String -> [HPR] -> Int
findNumberOfPoints i b hprs = sum $ map length $ selectData (checkAlmostEqYear i) hprs b

logStats :: Int -> Population Double -> IO ()
logStats iterno pop = do
    when (iterno == 0) $
        if verbose then putStrLn "# Generation best median worst verified?"
                   else putStrLn "Running (Each '.' represents a generation)"
    let gs = bestFirst Maximizing $ pop
    let (_,best) = head gs
    let (_,median) = gs !! (length gs `div` 2)
    let (_,worst) = last gs
    if verbose then putStrLn $ unwords [(show iterno), (take 5 $ show best), (take 5 $ show median), (take 5 $ show worst)]
               else do
                        putStr "."
                        hFlush stdout

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
        let details = tab ++ "Year Range in use: " ++ (take 4 baseDate) ++ " ± " ++ (show extraYears) ++ "\n" ++ tab ++ "Number of stocks: " ++ (show len) ++ "\n" ++ tab ++ "Number of correlations: " ++ (show $ length correlations) ++ "\n" ++ tab ++ "Time to run: " ++ (show timeLimit) ++ "s\n" ++ tab ++ "Population size: " ++ (show popsize) ++ "\n" ++ tab ++ "Elite Size: " ++ (show elitesize) ++ "\n" ++ tab ++ "Number of data Points: " ++ (show $ findNumberOfPoints extraYears baseDate hprs) ++ "\n" ++ tab ++ "Naive f value: " ++ (show $ f hprs correlations (naivef len)) ++ "\n"
        putStrLn $ "Init complete: \n" ++ details
        finalPop <- geneticAlgorithm len hprs correlations
        let (bestG, best) = head . bestFirst Maximizing $ finalPop
        putStrLn $ "\nFinished!\n" ++ tab ++ "Value achieved: " ++ (show best) ++ "\n" ++ tab ++"with f's: " ++ (show bestG)
        putStrLn details

--------------------------------------------

tem1 = zip [20,15,10,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] ["1998", "2003", "2008", "2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017"]


help = do
        all <- mapM temp $ tem1
        return $ map (\(x,y,h) -> findNumberOfPoints x y h) all

--temp :: (Integer, String) -> IO (Int, [HPR], Correlations)
temp (x,y) = do
                            p <- checkForErrors parseAll --see CSV
                            let hprs = makeAllHPR p --see CSV
                            --let cs = calcCorrelate hprs x y --see Risk
                            return $ (x, y, hprs)