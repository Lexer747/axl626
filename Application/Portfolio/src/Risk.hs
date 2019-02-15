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

--calculate the risk of a series of points
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
probK hprs cs i baseDate = product $ map (correlate n cs ps) ps
    where ps = map (\hpr -> (hpr,appliedP hpr i baseDate)) hprs
          n = 1 / ((fromIntegral $ length hprs) - 1)

correlate :: Double -> Correlations -> [(HPR,Double)] -> (HPR,Double) -> Double
correlate n cs hprs (baseH,baseD) = (product $ map f hprs) ** n
    where f (h,d) = ((getValue2 baseH h cs 1) * d * baseD) 

calcCorrelate :: [HPR] -> Integer -> String -> Correlations
calcCorrelate hprs i baseDate = mapMaybe inner allData
    where allData = createPermutations $ map (\h -> (h, g h, calcMean $ g h)) hprs
          g h = selectDataSingle (checkAlmostEqYear i) h baseDate
          inner ((_, _, _),(_, [], _))      = Nothing
          inner ((_, _, _),(_, [_], _))     = Nothing
          inner ((_, [], _),(_, _, _))      = Nothing
          inner ((_, [_], _),(_, _, _))     = Nothing
          inner ((h1, d1, m1),(h2, d2, m2)) = Just (h1, h2, calcCorrelatePure d1 d2 m1 m2 (0,0,0))

calcCorrelatePure :: [Double] -> [Double] -> 
                     Double -> Double -> 
                     (Double, Double, Double) -> Double
calcCorrelatePure [] _ _ _ (acc, accX, accY) = acc / (sqrt $ accX * accY)
calcCorrelatePure _ [] _ _ (acc, accX, accY) = acc / (sqrt $ accX * accY)
calcCorrelatePure (x:xs) (y:ys) meanX meanY (acc, accX, accY) = calcCorrelatePure xs ys meanX meanY (newAcc, newAccX, newAccY)
    where newAcc  = acc + ((x - meanX) * (y - meanY))
          newAccX = accX + ((x - meanX) ** 2)
          newAccY = accY + ((y - meanY) ** 2)

createPermutations :: [a] -> [(a,a)]
createPermutations [] = []
createPermutations (x:xs) = (zip (repeat x) xs) ++ (createPermutations xs)