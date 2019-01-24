module Risk where

import Types
import SemiDate

getWindow :: Integer -> Integer -> [Maybe a] -> [a]
getWindow _     _   []     = []
getWindow 0     0   (Just x:_)  = [x]
getWindow 0     0   (Nothing:_)  = []
getWindow 0     end (Just x:xs) = [x] ++ (getWindow 0 (end - 1) xs)
getWindow 0     end (Nothing:xs) = (getWindow 0 (end - 1) xs)
getWindow start end (_:xs) = getWindow (start - 1) (end - 1) xs

calcMean :: [Double] -> Double
calcMean xs = (sum xs) / (fromIntegral $ length xs)

calcVariance :: [Double] -> Double -> Double
calcVariance xs mean = let inner = map (\x -> (x - mean) ** 2) xs in
                       (sum inner) / (fromIntegral $ length xs)

getSameMonthAndYear :: String -> String -> Bool
getSameMonthAndYear 
