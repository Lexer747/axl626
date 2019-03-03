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
calcP :: [Double] -> Maybe Double
calcP []           = Nothing
calcP [_]          = Nothing
calcP [_,_]        = Nothing
calcP xs           = Just $ complCumulative (normalDistr mean (sqrt var)) 0
    where mean = calcMean xs
          var  = calcVariance xs mean

--find P<date> with a year size of i
--appliedP <stock> 0 1999-xx-xx 
appliedP :: HPR -> Integer -> String -> Maybe Double
appliedP hpr i baseDate = calcP toCalc
    where toCalc = selectDataSingle (checkAlmostEqYear i) hpr baseDate

probK :: [HPR] -> Correlations -> HPR -> Maybe Double
probK hprs cs h = correlate cs hprs h --no longer using ^ 1 / n -1

correlate :: Correlations -> [HPR] -> HPR -> Maybe Double
correlate cs hprs h = case risk h of
        Just r  -> Just (foldr f r hprs)
        Nothing -> Nothing
    where f h' prev = combineCorrelations prev (risk h') (getValue2 h' h cs)

combineCorrelations :: Double -> Maybe Double -> Maybe Double -> Double
combineCorrelations p1 (Just p2) (Just c) | c >= 0 = (p1 + (p2 * c)) / (1 + c)--(p1 + (p2 * c)) / 1 + c
combineCorrelations p1 (Just p2) (Just c)          = (p1 + ((1 - p2) * c')) / (1 + c')
    where c' = c * (- 1)
combineCorrelations p1 _  _                        = p1 --in the case either is nothing


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