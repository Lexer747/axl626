module Fundamentals where

import qualified Data.Vector as V

import CSV
import Types
import Risk

--given an input, strip out unnecessary data
getData :: (String, String, Fundamental) -> (String, String, [BaseData])
getData (p,n,vector) = (p,n,map parse list)
    where list = V.toList vector
          parse (d,c,_,_,_,o,_,_,_,_,_,_,_,_) = BaseData {date = d, close = read c, open = read o}

--perform the calculations, to find the biggest loss of the stock, and all trades
--note this does not find the risk, use completeHPR to do so
makeHPR :: (String, String, [BaseData]) -> HPR
makeHPR (_,_,[]) = error "makeHPR called with empty list, should have atleast one element"
makeHPR (p,n,(x:xs)) = makeHPR_help (p,n,xs) trade [(trade, date x)]
    where trade = (close x) - (open x)

--expected -Wmissing-fields
makeHPR_help :: (String, String, [BaseData]) -> Double -> [(Double, String)] -> HPR
makeHPR_help (p,n,(x:xs)) bl acc = case trade < bl of
        True  -> makeHPR_help (p,n,xs) trade ((trade, date x):acc)
        False -> makeHPR_help (p,n,xs) bl    ((trade, date x):acc)
    where trade = (close x) - (open x)
makeHPR_help (p,n,[])     bl acc = HPR {path = p, name = n,trades = acc, maxLoss = bl}

makeAllHPR :: [(String, String, Fundamental)] -> [HPR]
makeAllHPR = map (makeHPR . getData)

--given a HPR from makeHPR, we can fill in the risk field
completeHPR :: HPR -> Integer -> String -> HPR
completeHPR hpr i s = HPR {path = path hpr, name = name hpr, trades = trades hpr, maxLoss = maxLoss hpr, risk = r}
    where r = appliedP hpr i s

completeAllHPR :: [(String, String, Fundamental)] -> Integer -> String -> [HPR]
completeAllHPR xs i s = map (\h -> completeHPR h i s) $ makeAllHPR xs

--HPRk = (1 + (n Σ i=1 {fk * (-PLk,i / BLi) }) ) ^ Probk
innerG :: Integer -> String -> Double -> HPR -> Double
innerG i baseDate f hpr = if final < 0 then 0 else final
    -- ^ bound check, as if we happen to choose data which performed very badly we will
    -- have a negative, so instead just use 0
    where final = 1 + (sum $ map inner (selectDataSingle (checkAlmostEqYear i) hpr baseDate))
          inner x = f * ((- x) / (maxLoss hpr))


fullG :: Integer -> --Plus and minus the number of years to take data from
        String -> --The date to centralize the data collection from
        Correlations -> --A mapping of every stock to stock, and its correlation
                        --Its treated symmetrically so X -> Y = Z implies Y -> X = Z
                        --An empty mapping treats every stock as independent
        [(Double,HPR)] -> --A list of every pair of optimal f and stock
        Double --The return on invest for the parameters
fullG i baseDate cs hprs = probk-- g -- ** (1 / probk)
    where (g, probk) = partialG i baseDate cs hprs

partialG :: Integer -> String -> Correlations -> [(Double, HPR)] -> ([(Double, Maybe Double)], Double)
partialG i s cs fAndHprs = (zip (map inner fAndHprs) (map (probK hprs cs) hprs), 1)
    where inner (f,h) = innerG i s f h
          hprs = map snd fAndHprs
          --originalProbK = sum $ map (probK hprs cs) hprs

calcAnnum :: Integer -> Double -> Double
calcAnnum i gain = gain ** (1 / ((2 * n) + 1))
    where n = fromIntegral i