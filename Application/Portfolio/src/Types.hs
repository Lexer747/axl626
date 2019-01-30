module Types where

--adj_close (1), adj_open (5)
data BaseData = BaseData {date :: String, close :: Double, open :: Double}
    deriving (Show, Eq, Ord)

--store the data needed about a stock
data HPR = HPR {
        path :: String, 
        name:: String, 
        trades :: [(Double, String)], 
        maxLoss :: Double
    }

instance Eq HPR where
    (==) x y = (name x) == (name y)

instance Show HPR where
    show h = (name h) ++ " @ " ++ (path h) ++ "\n BL:" ++ (show $ maxLoss h) ++ "\n" ++ (show $ trades h) ++ "\n"

type Correlations = [(HPR,HPR,Double)]

getValue :: Eq a => a -> [(a,b)] -> b -> b
getValue x ((a,b):_) _ | (x == a) = b
getValue x (_:bs) b               = getValue x bs b
getValue _ [] b                 = b

getValue2 :: Eq a => a -> a -> [(a,a,b)] -> b -> b
getValue2 x y ((a,a',b):_) _ | (x == a) && (y == a') = b
getValue2 x y ((a,a',b):_) _ | (x == a') && (y == a) = b
getValue2 x y (_:bs) b                               = getValue2 x y bs b
getValue2 _ _ [] b                                   = b



{-
data G = G {
        stocks :: [HPR],
        setPL :: Integer -> Integer -> Maybe Double,
        setBL :: Integer -> Maybe Double,
        setF :: Integer -> Maybe Double,
        setP :: Integer -> Integer -> Integer -> Maybe Double,
        n :: Integer -> Maybe Integer, -- n 0 = length $ trades (stocks !! 0)
        m :: Integer -- m = length stocks
    }
-}