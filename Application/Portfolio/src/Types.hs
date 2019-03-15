module Types (
        BaseData(..),
        HPR(..),
        Correlations,
        getValue2
    ) where

--adj_close (1), adj_open (5)
data BaseData = BaseData {date :: String, close :: Double, open :: Double}
    deriving (Show, Eq, Ord)

--store the data needed about a stock
data HPR = HPR {
        path :: String,
        name:: String,
        trades :: [(Double, String)],
        maxLoss :: Double,
        risk :: Maybe Double
    }

instance Eq HPR where
    (==) x y = (name x) == (name y)

instance Show HPR where
    show h = summariseHPR 0 h
    --show h = (name h) ++ " @ " ++ (path h) ++ "\n BL:" ++ (show $ maxLoss h) ++ "\n" ++ (show $ trades h) ++ "\n"

type Correlations = [(HPR,HPR,Double)]

--Helper functions for working with maps encoded as lists of pairs
getValue :: Eq a => a -> [(a,b)] -> b -> b
getValue x ((a,b):_) _ | (x == a) = b
getValue x (_:bs) b               = getValue x bs b
getValue _ [] b                 = b

getValue2 :: Eq a => a -> a -> [(a,a,b)] -> Maybe b
getValue2 x y ((a,a',b):_) | (x == a) && (y == a') = Just b --X -> Y = Z
getValue2 x y ((a,a',b):_) | (x == a') && (y == a) = Just b --Y -> X = Z
getValue2 x y (_:bs)                               = getValue2 x y bs
getValue2 _ _ []                                   = Nothing


summariseHPRS :: [HPR] -> String
summariseHPRS = summariseHPRS_help 0

summariseHPRS_help :: Int -> [HPR] -> String
summariseHPRS_help _ []     = []
summariseHPRS_help i (h:hs) = (summariseHPR i h) ++ "\n" ++(summariseHPRS_help (i + 1) hs)

summariseHPR :: Int -> HPR -> String
summariseHPR i h = (show i) ++ ". \"" ++ (name h) ++ "\"; BL = " ++ (show $ maxLoss h) ++ ";" ++ (succinctList $ trades h)

succinctList :: (Show a) => [a] -> String
succinctList xs | length xs <= 4 = show xs
succinctList xs | otherwise      = "[" ++ (show $ xs !! 0) ++ "," ++ (show $ xs !! 1) ++ ",...," ++ (show $ xs' !! 1) ++ "," ++ (show $ xs' !! 0) ++ "]"
    where xs' = reverse xs

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