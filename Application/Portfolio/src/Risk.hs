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
selectData f xs base = map (\h -> selectDataSingle f h base) xs

selectDataSingle :: (String -> String -> Bool) -> HPR -> String -> [Double]
selectDataSingle f h base = mapMaybe g (trades h)
    where g (x,d) = case (f base d) of
                        True -> Just x
                        False -> Nothing

calcP :: [Double] -> Double
calcP xs = complCumulative (normalDistr mean (sqrt var)) 0
    where mean = calcMean xs
          var  = calcVariance xs mean

