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

calcMean :: [Double] -> Double -> Double
calcMean xs len = (sum xs) / len

--standard deviation = sqrt(variance)
calcVariance :: [Double] -> Double -> Double -> Double
calcVariance xs len mean = let inner x = (x - mean) ** 2 in
                           let total = sum $ map inner xs in
                           total / (len - 1)

generateP :: [Double] -> 

makeG :: [HPR] -> G
makeG xs = G {
        stocks = xs,
        m = fromIntegral $ length xs,
        n = (\x -> getN xs (fromIntegral x)),
        setPL = (\k i -> getPL xs k i),
        setBL = (\i -> getBL xs i),
        setF = getF (fromIntegral $ length xs),
        setP = (\_ _ _ -> Nothing)
    }

getN :: [HPR] -> Integer -> Maybe Integer
getN []     _ = Nothing
getN (x:_)  0 = Just $ fromIntegral $ length $ trades x
getN (_:xs) k = getN xs (k - 1)

getF :: Integer -> Integer -> Maybe Double
getF m k = if (k > m)
              then Nothing
              else Just 0.5

getPL :: [HPR] -> Integer -> Integer -> Maybe Double
getPL []     _ _ = Nothing
getPL (x:_)  0 i = getPL_help (trades x) i
getPL (_:xs) k i = getPL xs (k - 1) i

getPL_help :: [(Double,String)] -> Integer -> Maybe Double
getPL_help []        _ = Nothing
getPL_help ((x,_):_) 0 = Just x
getPL_help (_:xs)    i = getPL_help xs (i - 1)

getBL :: [HPR] -> Integer -> Maybe Double
getBL []     _ = Nothing
getBL (x:_)  0 = Just $ maxLoss x
getBL (_:xs) k = getBL xs k

-- P(ik | jk)
-- Probability of the scenario i having the k outcome given
-- the probability of the scenario j having the k outcome
getP :: [HPR] 

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
            return $ makeG $ makeAllHPR p