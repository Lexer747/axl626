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
selectData f xs baseDate = filter (not . null) $ map (\h -> selectDataSingle f h baseDate) xs

selectDataSingle :: (String -> String -> Bool) -> HPR -> String -> [Double]
selectDataSingle f h baseDate = mapMaybe g (trades h)
    where g (x,d) = case (f baseDate d) of
                        True -> Just x
                        False -> Nothing

calcP :: [Double] -> Double
calcP []  = 1 --variance of an empty distribution is NaN, so make it the neutral for multiplication
calcP [_] = 1 --variance of a single element distribution is 0, so make it the neutral for multiplication
calcP xs  = complCumulative (normalDistr mean (sqrt var)) 0
    where mean = calcMean xs
          var  = calcVariance xs mean

--find P<date> with a year size of i
--appliedP <stock> 0 1999-xx-xx 
appliedP :: HPR -> Integer -> String -> Double
appliedP hpr i baseDate = calcP toCalc
    where toCalc = selectDataSingle (checkAlmostEqYear i) hpr baseDate

--Probk = (n - 1 Π i=1 {n Π j=i+1 { P(ik | jk) }}) ^ (1 / (n - 1)) 
probK :: [HPR] -> Correlations -> Integer -> String -> Double
probK hprs cs i baseDate = inner ** (1 / (n - 1))
    where ps = map (\hpr -> (hpr,appliedP hpr i baseDate)) hprs
          inner = product $ map (correlate cs ps) ps
          n = fromIntegral $ length hprs

correlate :: Correlations -> [(HPR,Double)] -> (HPR,Double) -> Double
correlate cs hprs (baseH,baseD) = product $ map f hprs
    where f (h,d) = (getValue2 baseH h cs 1) * d * baseD