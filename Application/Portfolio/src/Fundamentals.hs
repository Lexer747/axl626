module Fundamentals where

import qualified Data.Vector as V

import CSV

--adj_close (1), adj_open (5)
data BaseData = BaseData {close :: Double, open :: Double}
    deriving (Show, Eq, Ord)

data HPR = HPR {trades :: [Double], maxLoss :: Double}
    deriving (Show, Eq, Ord)

getData :: Fundamental -> [BaseData]
getData vector = map parse list
    where list = V.toList vector
          parse (_,c,_,_,_,o,_,_,_,_,_,_,_,_) = BaseData {close = read c, open = read o}

makeHPR :: [BaseData] -> HPR
makeHPR []     = error "makeHPR called with empty list, should have atleast one element"
makeHPR (x:xs) = makeHPR_help xs trade [trade]
    where trade = (close x) - (open x)

makeHPR_help :: [BaseData] -> Double -> [Double] -> HPR
makeHPR_help (x:xs) bl acc = case trade < bl of
        True  -> makeHPR_help xs trade (trade:acc)
        False -> makeHPR_help xs bl    (trade:acc)
    where trade = (close x) - (open x)
makeHPR_help []     bl acc = HPR {trades = acc, maxLoss = bl}



--HPR_k = (1 + (n Σ i=1 {f_i * (-PL_k,i / BL_i) }) ) ^ Prob_k
calcHPR_k :: Double -> Double -> HPR -> Double
calcHPR_k f prob_k set = let inner pl = f * (pl / (maxLoss set)) in
                         let total = sum $ map inner (trades set) in
                         (1 + total) ** prob_k

--Prob_k = (n - 1 Π i=1 {n Π j=i+1 { P(i_k | j_k) }}) ^ (1 / (n - 1)) 
--calcProb_k :: 

calcMean :: [Double] -> Double -> Double
calcMean xs len = (sum xs) / len

calcVariance :: [Double] -> Double -> Double -> Double
calcVariance xs len mean = let inner x = (x - mean) ** 2 in
                       let total = sum $ map inner xs in
                       total / (len - 1)
















