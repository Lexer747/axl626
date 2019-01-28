module Risk where

import Data.Maybe (mapMaybe)
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution (Distribution(..))

import Types
import SemiDate

calcMean :: [Double] -> Double
calcMean xs = (sum xs) / (fromIntegral $ length xs)

calcVariance :: [Double] -> Double -> Double
calcVariance xs mean = let inner = map (\x -> (x - mean) ** 2) xs in
                       (sum inner) / (fromIntegral $ length xs)

checkAlmostEqYear :: Integer -> String -> String -> Bool
checkAlmostEqYear i = findBasedOnDate [((almostEq i),Year)] (&&)

selectData :: (String -> String -> Bool) -> [HPR] -> String -> [[Double]]
selectData f xs baseDate = map (\h -> selectDataSingle f h baseDate) xs

selectDataSingle :: (String -> String -> Bool) -> HPR -> String -> [Double]
selectDataSingle f h baseDate = mapMaybe g (trades h)
    where g (x,d) = case (f baseDate d) of
                        True -> Just x
                        False -> Nothing

calcP :: [Double] -> Double
calcP xs = complCumulative (normalDistr mean (sqrt var)) 0
    where mean = calcMean xs
          var  = calcVariance xs mean

appliedP :: HPR -> Integer -> String -> Double
appliedP hpr i baseDate = calcP toCalc
    where toCalc = selectDataSingle (checkAlmostEqYear i) hpr baseDate

