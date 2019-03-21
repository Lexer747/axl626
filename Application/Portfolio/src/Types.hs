module Types (
        BaseData(..),
        HPR(..),
        Correlations,
        getValue,
        getValue2
    ) where

import Control.DeepSeq

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

-- Default implementation to allow parallel execution on this type
instance NFData HPR where
    rnf a = a `seq` ()

type Correlations = [(HPR,HPR,Double)]

--Helper functions for working with maps encoded as lists of pairs
getValue :: Eq a => a -> [(a,b)] -> b -> b
getValue x ((a,b):_) _ | (x == a) = b
getValue x (_:bs) b               = getValue x bs b
getValue _ [] b                 = b

--Another helper function for working with tupled maps as list of 3 pairs
getValue2 :: Eq a => a -> a -> [(a,a,b)] -> Maybe b
getValue2 x y ((a,a',b):_) | (x == a) && (y == a') = Just b --X -> Y = Z
getValue2 x y ((a,a',b):_) | (x == a') && (y == a) = Just b --Y -> X = Z
getValue2 x y (_:bs)                               = getValue2 x y bs
getValue2 _ _ []                                   = Nothing

-- Utility function for pretty printing
summariseHPR :: Int -> HPR -> String
summariseHPR i h = (show i) ++ ". \"" ++ (name h) ++ "\"; BL = " ++ (show $ maxLoss h) ++ ";" ++ (succinctList $ trades h)

succinctList :: (Show a) => [a] -> String
succinctList xs | length xs <= 4 = show xs
succinctList xs | otherwise      = "[" ++ (show $ xs !! 0) ++ "," ++ (show $ xs !! 1) ++ ",...," ++ (show $ xs' !! 1) ++ "," ++ (show $ xs' !! 0) ++ "]"
    where xs' = reverse xs