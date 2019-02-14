module Fundamentals where

import qualified Data.Vector as V

import CSV
import Types
import SemiDate
import Risk

--given an input, strip out unnecessary data
getData :: (String, String, Fundamental) -> (String, String, [BaseData])
getData (p,n,vector) = (p,n,map parse list)
    where list = V.toList vector
          parse (d,c,_,_,_,o,_,_,_,_,_,_,_,_) = BaseData {date = d, close = read c, open = read o}

--perform the calculations, to find the biggest loss of the stock, and all trades
makeHPR :: (String, String, [BaseData]) -> HPR
makeHPR (_,_,[]) = error "makeHPR called with empty list, should have atleast one element"
makeHPR (p,n,(x:xs)) = makeHPR_help (p,n,xs) trade [(trade, date x)]
    where trade = (close x) - (open x)

makeHPR_help :: (String, String, [BaseData]) -> Double -> [(Double, String)] -> HPR
makeHPR_help (p,n,(x:xs)) bl acc = case trade < bl of
        True  -> makeHPR_help (p,n,xs) trade ((trade, date x):acc)
        False -> makeHPR_help (p,n,xs) bl    ((trade, date x):acc)
    where trade = (close x) - (open x)
makeHPR_help (p,n,[])     bl acc = HPR {path = p, name = n,trades = acc, maxLoss = bl}

makeAllHPR :: [(String, String, Fundamental)] -> [HPR]
makeAllHPR = map (makeHPR . getData)

getBL :: HPR -> Double
getBL hpr = case xs of
                [] -> error "empty list"
                xs' -> foldr1 min [x | (x,_) <- xs']
    where xs = (trades hpr)

--HPRk = (1 + (n Σ i=1 {fk * (-PLk,i / BLi) }) ) ^ Probk
innerG :: Integer -> String -> Double -> Double -> HPR -> Double
innerG i baseDate probk f hpr = final ** probk
    where final = 1 + (sum $ map inner (selectDataSingle (checkAlmostEqYear i) hpr baseDate))
          inner x = f * (x / (maxLoss hpr))


fullG :: Integer -> --Plus and minus the number of years to take data from
        String -> --The date to centralize the data collection from
        Correlations -> --A mapping of every stock to stock, and its correlation
                        --Its treated symmetrically so X -> Y = Z implies Y -> X = Z
                        --An empty mapping treats every stock as independent
        [(Double,HPR)] -> --A list of every pair of optimal f and stock
        Double --The return on invest for the parameters
fullG i baseDate cs hprs = (product $ map inner hprs)
    where inner (f,h) = innerG i baseDate probk f h
          probk = probK [x | (_,x) <- hprs] cs i baseDate

test = do
        p <- checkForErrors parseAll
        return $ fullG 1 "1999-xx-xx" [] $ zip (repeat 0.3) (makeAllHPR p)

test2 = do
        p <- checkForErrors parseAll
        return $ head $ reverse $ calcCorrelate (makeAllHPR p) 1 "1999-xx-xx"
