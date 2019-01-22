module Fundamentals where

import qualified Data.Vector as V

import CSV
import Types
import SemiDate

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
        setF = getF (fromIntegral $ length xs)
    }

getN :: [HPR] -> Integer -> Maybe Integer
getN []     _ = Nothing
getN (x:_)  0 = Just $ fromIntegral $ length $ trades x
getN (_:xs) k = getN xs (k - 1)

getF :: Integer -> Integer -> Maybe Double
getF m k = if (k > m)
              then Nothing
              else Just 0.5

--getPL :: [HPR] -> 

findMinMaxDate :: [HPR] -> (String, String)
findMinMaxDate []     = error "no HPR's to compare"
findMinMaxDate (x:xs) = foldr findMinMaxDate_help (i,i) ys
    where (_,i) = head $ trades x
          ys       = map trades (x:xs)

findMinMaxDate_help :: [(Double,String)] -> (String, String) -> (String, String)
findMinMaxDate_help []         (curMin, curMax) = (curMin, curMax)
findMinMaxDate_help ((_,x):xs) (curMin, curMax) = 
    if sortDate (>) x curMax
        then findMinMaxDate_help xs (curMin, x)
        else if sortDate (<) x curMin
                then findMinMaxDate_help xs (x, curMax)
                else findMinMaxDate_help xs (curMin,curMax)


test = do
            p <- checkForErrors parseAll
            return $ makeAllHPR p