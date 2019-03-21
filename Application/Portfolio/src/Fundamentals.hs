module Fundamentals (
        completeAllHPR,
        fullG,
        decoupleG,
        decoupleR,
        calcAnnum
    ) where

import qualified Data.Vector as V
import Data.Maybe (maybeToList)

import CSV
import Types
import Risk
import ParallelUtils

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

--Map over all CSV files and make all of them into the HPR representation
makeAllHPR :: [(String, String, Fundamental)] -> [HPR]
makeAllHPR = parallelMap (makeHPR . getData)

--given a HPR from makeHPR, we can fill in the risk field
completeHPR :: HPR -> Integer -> String -> HPR
completeHPR hpr i s = HPR {path = path hpr, name = name hpr, trades = trades hpr, maxLoss = maxLoss hpr, risk = r}
    where r = appliedP hpr i s

completeAllHPR :: [(String, String, Fundamental)] -> Integer -> String -> [HPR]
completeAllHPR xs i s = parallelMap (\h -> completeHPR h i s) $ makeAllHPR xs

--HPRk = (1 + (n Σ i=1 {fk * (-PLk,i / BLi) }) ) ^ Probk
innerG :: Integer -> String -> Double -> HPR -> Maybe Double
innerG i baseDate f hpr = case (final input) of
        Just x | x < 0 -> Just 0
        Just x         -> Just x
        Nothing        -> Nothing
    -- ^ bound check, as if we happen to choose data which performed very badly we will
    -- have a negative, so instead just use 0
    where final [] = Nothing
          final [_] = Nothing
          final [_,_] = Nothing
          final [_,_,_] = Nothing
          -- if we don't have enough data ^
          final xs = Just $ 1 + (sum $ map inner xs)
          input = (selectDataSingle (checkAlmostEqYear i) hpr baseDate)
          inner x = f * ((- x) / (maxLoss hpr))


fullG :: Integer -> --Plus and minus the number of years to take data from
        String -> --The date to centralize the data collection from
        Correlations -> --A mapping of every stock to stock, and its correlation
                        --Its treated symmetrically so X -> Y = Z implies Y -> X = Z
                        --An empty mapping treats every stock as independent
        [(Double,HPR)] -> --A list of every pair of optimal f and stock
        Double --The return on invest for the parameters
fullG i baseDate cs hprs = (inner g p) ** (1 / probk)
    where (g, p) = partialG i baseDate cs hprs
          probk = sum $ concatMap maybeToList p
          inner :: [Maybe Double] -> [Maybe Double] -> Double
          inner [] []                         = 1
          inner ((Just hpr):hs) ((Just k):ks) = (hpr ** k) * (inner hs ks)
          inner (Nothing:hs) (_:ks)           = inner hs ks
          inner (_:hs) (Nothing:ks)           = inner hs ks
          inner _ _                           = error "fullG: inner fail, mismatched list length"

partialG :: Integer -> String -> Correlations -> [(Double, HPR)] -> ([Maybe Double], [Maybe Double])
partialG i s cs fAndHprs = ((parallelMap inner fAndHprs), (parallelMap (probK hprs cs) hprs))
    where inner (f,h) = innerG i s f h
          hprs = map snd fAndHprs

decoupleG :: Integer -> String -> Correlations -> [(Double, HPR)] -> Double
decoupleG i s _ fAndHprs = product $ concatMap maybeToList $ parallelMap inner fAndHprs
    where inner (f,h) = innerG i s f h

decoupleR :: Integer -> String -> Correlations -> [(Double, HPR)] -> Double
decoupleR _ _ cs fAndHprs = product $ parallelMapMaybe inner fAndP
    where fAndP = zip (map fst fAndHprs) maybes
          inner (f, (Just p)) = Just $ 1 + (f * p)
          inner (_, Nothing)  = Nothing
          maybes = map (probK hprs cs) hprs
          hprs = map snd fAndHprs


normalize :: RealFrac a => a -> a -> a -> a -> a -> a
normalize curMin curMax _ _ _ | curMin == curMax = error "normalize called with equal old min max"
normalize curMin curMax newMin newMax cur = (((newMax - newMin) * (cur - curMin)) / (curMax - curMin)) + newMin

transform :: [Double] -> [Double]
transform [] = []
transform (x:xs) = map (normalize curMin curMax 0 1) xs where
    curMin = foldr min x xs
    curMax = foldr max x xs

calcAnnum :: Integer -> String -> [(Double, HPR)] -> Double
calcAnnum i s fAndHprs = ((sum $ map inner fAndHprs) / window) / (sum $ map fst fAndHprs) where
    inner (f,hpr) = case prices hpr of
                        [] -> 0
                        [_] -> 0
                        xs -> f * (sum $ transform xs)
    prices h = selectDataSingle (checkAlmostEqYear i) h s
    window = fromIntegral $ (2 * i) + 1