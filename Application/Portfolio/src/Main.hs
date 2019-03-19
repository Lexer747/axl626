module Main (main) where

import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Random (getRandomR, getNormal, withProbability)
import Moo.GeneticAlgorithm.Multiobjective

import Control.Monad (replicateM, when)
import System.IO
import System.Environment (getArgs)
import System.Exit
import Text.Printf

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
toCSV = True

multiObjective :: Bool
multiObjective = True

------------- CALC CORRELATIONS ------------

initCorrelationsAndData :: Integer -> String -> IO (Int, [HPR], Correlations)
initCorrelationsAndData i s = do
                            p <- checkForErrors parseAll --see CSV
                            let hprs = completeAllHPR p i s --see CSV
                            let cs = calcCorrelate hprs i s --see Risk
                            return $ (length hprs, hprs, cs)

------------- GENETIC CONSTANTS ------------

timeLimit :: Double
timeLimit = 20 * 60 --seconds

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

-- Our function we want to optimize, its fullG partially applied with the stocks and correlations
f :: Integer -> String -> [HPR] -> Correlations -> [Double] -> Double
f i s hprs cs xs = if isNaN g then error $ "NaN caused by: " ++ (show xs)
                          else g
    where g = fullG i s cs (zip xs hprs) --see Fundamentals

partialf :: Integer -> String -> [HPR] -> Correlations -> [Double] -> (Double, Double)
partialf i s hprs cs xs = (decoupleG i s cs (zip xs hprs), decoupleR i s cs (zip xs hprs))

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
step :: Integer -> String -> [HPR] -> Correlations -> StepGA Rand Double
step i s hprs cs = nextGeneration Maximizing (f i s hprs cs) select elitesize crossover mutate

multiObjectiveProblem :: Integer -> String -> [HPR] -> Correlations -> MultiObjectiveProblem ([Double] -> Double)
multiObjectiveProblem i s hprs cs = [(Maximizing,(\xs -> decoupleG i s cs (zip xs hprs))),(Minimizing,(\xs -> decoupleR i s cs (zip xs hprs)))]

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

verfiy :: [Double] -> Bool
verfiy xs = (sum xs <= 1) && (sum xs >= 0) && (foldr (&&) True (map ((<) 0) xs))
-- (show $ foldr (&&) True $ map (\ (a,_) -> verfiy a) gs)

findNumberOfPoints :: Integer -> String -> [HPR] -> Int
findNumberOfPoints i b hprs = sum $ map length $ selectData (checkAlmostEqYear i) hprs b

findBest :: [MultiPhenotype Double] -> ((Genome Double, [Objective]), (Genome Double, [Objective]))
findBest []     = error "Empty population"
findBest ((a,x):xs) = findBest_help xs (a,x) (a,x)

findBest_help :: [MultiPhenotype Double] -> (Genome Double, [Objective]) -> (Genome Double, [Objective]) -> ((Genome Double, [Objective]), (Genome Double, [Objective]))
findBest_help ((a,[g,r]):xs) (_,[bestG,_])  (_,[bestR,_])  | ((g > bestG) && (r < bestR)) = findBest_help xs (a,[g,r]) (a,[g,r])
findBest_help ((a,[g,r]):xs) (_,[bestG,_])  (c,[bestR,r']) | (g > bestG)                  = findBest_help xs (a,[g,r]) (c,[bestR,r'])
findBest_help ((a,[g,r]):xs) (b,[bestG,g']) (_,[bestR,_])  | (r < bestR)                  = findBest_help xs (b,[bestG,g']) (a,[g,r])
findBest_help (_:xs)         g'             r'                                            = findBest_help xs g' r'
findBest_help []             g'             r'                                            = (g',r')

logStats :: Integer -> String -> [HPR] -> Correlations -> Int -> Population Double -> IO ()
logStats i s hprs cs iterno pop = do
    when (iterno == 0) $
        if verbose then (if multiObjective 
                            then putStrLn "# Generation best"
                            else putStrLn "# Generation best median worst")
                   else putStrLn "Running (Each '.' represents a generation)"
    if multiObjective 
        then (if verbose 
            then (let res = evalAllObjectives (multiObjectiveProblem i s hprs cs) pop in
                  let ((g,bestG),(r, bestR)) = findBest res in
                  putStrLn $ unwords $ [(show iterno), "Highest Gain:", (show bestG), "Lowest Risk:", (show bestR)])
            else do
                     putStr "."
                     hFlush stdout)
        else (let gs = bestFirst Maximizing $ pop in
              let (_,best) = head gs in
              let (_,median) = gs !! (length gs `div` 2) in
              let (_,worst) = last gs in
              if verbose then putStrLn $ unwords [(show iterno), (take 5 $ show best), (take 5 $ show median), (take 5 $ show worst)]
                           else do
                                    putStr "."
                                    hFlush stdout)

geneticAlgorithm :: Integer -> String -> Int -> [HPR] -> Correlations -> IO (Population Double)
geneticAlgorithm i s genomeLen hprs cs = do
    runIO (initGenome genomeLen) innerLoop
    where innerLoop = loopIO
                    [DoEvery 1 (logStats i s hprs cs), (TimeLimit timeLimit)]
                    (Generations maxIterations)
                    (if multiObjective then (multiObjectiveStep i s hprs cs)
                                       else (step i s hprs cs))

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
        let (naiveG, naiveR) = partialf i s hprs correlations (naivef len)
        let details = tab ++ "\
\Year Range in use: " ++ (take 4 s) ++ " ± " ++ (show i) ++ "\n" ++ tab ++ "\
\Number of stocks: " ++ (show len) ++ "\n" ++ tab ++ "\
\Number of correlations: " ++ (show $ length correlations) ++ "\n" ++ tab ++ "\
\Time to run: " ++ (show timeLimit) ++ "s\n" ++ tab ++ "\
\Population size: " ++ (show popsize) ++ "\n" ++ tab ++ "\
\MultiObjective?: " ++ (show multiObjective) ++ "\n" ++ tab ++ "\
\Elite Size: " ++ (show elitesize) ++ "\n" ++ tab ++ "\
\Number of data Points: " ++ (show $ findNumberOfPoints i s hprs) ++ "\n" ++ tab ++ "\
\Naive f value: " ++ (show $ f i s hprs correlations (naivef len)) ++ "\n" ++ tab ++ "\
\Per Annum value: " ++ (show $ calcAnnum i $ f i s hprs correlations (naivef len)) ++ "\n" ++ tab ++ "\
\Decoupled | Gain: " ++ (show naiveG) ++ " | Risk: " ++ (show naiveR) ++ "\n"

        putStrLn $ "Init complete: \n" ++ details
        finalPop <- geneticAlgorithm i s len hprs correlations
        let bestG =  if multiObjective 
                            then fst $ fst (findBest $ evalAllObjectives (multiObjectiveProblem i s hprs correlations) finalPop)
                            else fst (head . bestFirst Maximizing $ finalPop)
        let total = f i s hprs correlations bestG
        let (g, r) = partialf i s hprs correlations bestG
        putStrLn $ "\n\
\Finished!\n" ++ tab ++ "\
\Value achieved: " ++ (show total) ++ "\n" ++ tab ++"\
\Per Anumn: " ++ (show $ calcAnnum i total) ++ "\n" ++ tab ++ "\
\Decoupled | Gain: " ++ (show g) ++ " | Risk: " ++ (show r) ++ "\n" ++ tab ++ "\
\with f's: " ++ (show bestG)
        putStrLn details
        when toCSV $ do
            mapM (\x -> printf "%.5f\n" x) bestG
            putStrLn ""
            printf "%.8f\n" total
            putStrLn ""
            foldMap (\(g:r:[]) -> printf "%.7f , %.7f\n" g r) $ map snd $ evalAllObjectives (multiObjectiveProblem i s hprs correlations) finalPop
