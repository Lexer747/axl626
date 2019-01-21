module Fundamentals where

import qualified Data.Vector as V

import CSV
import Types

--given an input, strip out unnecessary data
getData :: Fundamental -> [BaseData]
getData vector = map parse list
    where list = V.toList vector
          parse (d,c,_,_,_,o,_,_,_,_,_,_,_,_) = BaseData {date = d, close = read c, open = read o}

--perform the calculations, to find the biggest loss of the stock, and all trades
makeHPR :: [BaseData] -> HPR
makeHPR []     = error "makeHPR called with empty list, should have atleast one element"
makeHPR (x:xs) = makeHPR_help xs trade [(trade, date x)]
    where trade = (close x) - (open x)

makeHPR_help :: [BaseData] -> Double -> [(Double, String)] -> HPR
makeHPR_help (x:xs) bl acc = case trade < bl of
        True  -> makeHPR_help xs trade ((trade, date x):acc)
        False -> makeHPR_help xs bl    ((trade, date x):acc)
    where trade = (close x) - (open x)
makeHPR_help []     bl acc = HPR {trades = acc, maxLoss = bl}

makeAllHPR :: [Fundamental] -> [HPR]
makeAllHPR = map (makeHPR . getData)

--HPR_k = (1 + (n Σ i=1 {f_i * (-PL_k,i / BL_i) }) ) ^ Prob_k
calcHPR_k :: Double -> Double -> HPR -> Double
calcHPR_k f prob_k set = let inner (pl,_) = f * (pl / (maxLoss set)) in
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

makeG :: [HPR] -> G
makeG xs = G {
        stocks = xs,
        m = fromIntegral $ length xs,
        n = (\x -> getN xs (fromIntegral x)),
        setPL = (\_ _ -> Nothing),
        setBL = (\_ -> Nothing),
        setF = (\k -> Nothing)
    }

getN :: [HPR] -> Integer -> Maybe Integer
getN []     _ = Nothing
getN (x:xs) 0 = Just $ fromIntegral $ length $ trades x
getN (x:xs) n = getN xs n

getF

test = do
            p <- checkForErrors parseAll
            return $ makeAllHPR p