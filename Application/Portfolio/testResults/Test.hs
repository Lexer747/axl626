module Test where

import Data.Maybe (mapMaybe)
import Data.List.Split
import Text.Printf

f :: String -> IO [String]
f path = do
    file <- readFile path
    return $ lines $ filter (\c -> not $ c `elem` badChars ) file

badChars :: [Char]
badChars = ['\160','\9632','\9618','\r','\NUL','\9516']

files :: [String]
files = map (\n -> "raw" ++ (show n) ++ ".csv") [0..9 :: Int]

numFiles :: Int
numFiles = length files

allCSV :: IO ()
allCSV = do
    xss <- mapM f files
    let out = interweave xss
    writeFile "temp.csv" $ unlines out

safeHd :: [a] -> Maybe a
safeHd (x:_) = Just x
safeHd []    = Nothing

safeTl :: [a] -> Maybe [a]
safeTl (_:xs) = Just xs
safeTl []     = Nothing

interweave :: [[a]] -> [a]
interweave xs = (mapMaybe safeHd xs) ++ (recCall)
    where recCall = case mapMaybe safeTl xs of
                        [] -> []
                        x -> interweave x

full :: IO ()
full = do
        s <- f "full.csv"
        let out = averagefs $ gatherfs s
        mapM_ (\(a,(x:y:z:[]),(g:gr:r:[])) -> do
            printf "%s, %.9f, %.9f, %.9f, " a x y z
            mapM_ (\i -> printf "%.5f, " i) g
            mapM_ (\i -> printf "%.5f, " i) gr
            mapM_ (\i -> printf "%.5f, " i) r
            printf "\n"
                                                ) out

averagefs :: [(String, [String])] -> [(String, [Double], [[Double]])]
averagefs [] = []
averagefs ((date, fs):fss) = (date, [varG, varGR, varR],[bestG, bestGR, bestR]):(averagefs fss)
    where (bestG,varG) = pullout $ calc $ reads $ take numFiles fs
          (bestGR,varGR) = pullout $ calc $ reads $ take numFiles $ drop numFiles fs
          (bestR, varR) = pullout $ calc $ reads $ take numFiles $ drop numFiles $ drop numFiles fs
          pullout :: [(Double,Double)] -> ([Double],Double)
          pullout xs = (map fst xs, calcVariance (map snd xs) (calcMean $ map snd xs))
          calc :: [[Double]] -> [(Double, Double)]
          calc xs = map (\x -> (calcMean x, calcVariance x (calcMean x))) xs
          reads :: [String] -> [[Double]]
          reads xs = map (map read) $ chunksOf numFiles $ interweave $ map (splitOn ", ") xs

gatherfs ::[String] -> [(String, [String])]
gatherfs [] = []
gatherfs strs@(_:_) = [(date,(interweave $ map snd $ fs))] ++ (gatherfs $ drop numFiles strs)
    where date = fst $ head fs
          fs = map takefs hd
          hd = take numFiles strs

takefs :: String -> (String, [String])
takefs str = (head splitted, map (\x -> reverse $ drop 3 $ reverse $ drop 1 x) fs)
    where fs = reverse $ take 3 $ reverse splitted
          splitted = splitOn "; " str

calcMean :: [Double] -> Double
calcMean xs = (sum xs) / (fromIntegral $ length xs)

calcVariance :: [Double] -> Double -> Double
calcVariance xs mean = let inner = map (\x -> (x - mean) ** 2) xs in
                       (sum inner) / (fromIntegral $ length xs)