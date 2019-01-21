module Types where

--adj_close (1), adj_open (5)
data BaseData = BaseData {date :: String, close :: Double, open :: Double}
    deriving (Show, Eq, Ord)

--store the data needed about a stock
data HPR = HPR {trades :: [(Double, String)], maxLoss :: Double}
    deriving (Show, Eq, Ord)

data G = G {
        stocks :: [HPR],
        setPL :: Integer -> Integer -> Maybe Double,
        setBL :: Integer -> Maybe Double,
        setF :: Integer -> Maybe Double,
        n :: Integer -> Maybe Integer, -- n 0 = length $ trades (stocks !! 0)
        m :: Integer -- m = length stocks
    }
