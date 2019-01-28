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

getBL :: HPR -> Maybe Double
getBL hpr = case xs of
                [] -> Nothing
                xs' -> Just $ foldr1 min [x | (x,_) <- xs']
    where xs = (trades hpr)





test = do
            p <- checkForErrors parseAll
            return $ makeAllHPR p