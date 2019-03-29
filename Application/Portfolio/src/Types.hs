module Types (
        BaseData(..),
        HPR(..),
        Correlations,
        getCorrelation,
    ) where

import Control.DeepSeq
import Data.HashMap.Strict
import Data.Hashable
import Prelude hiding (lookup)

--adj_close (1), adj_open (5)
data BaseData = BaseData {date :: String, close :: Double, open :: Double}
    deriving (Show, Eq, Ord)

--store the data needed about a stock
data HPR = HPR {
        path :: String,
        name:: String,
        trades :: [(Double, String)],
        prices :: [(Double, Double, String)],
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

--Hash a HPR on the name of it; therefore all stocks must have unique names
instance Hashable HPR where
    hash h = hash $ name h
    hashWithSalt i h = hashWithSalt i $ name h

type Correlations = HashMap (HPR,HPR) Double

getCorrelation :: HPR -> HPR -> Correlations -> Maybe Double
getCorrelation h h' cs = case lookup (h,h') cs of
    Just d -> Just d
    Nothing -> lookup (h',h) cs

-- Utility function for pretty printing
summariseHPR :: Int -> HPR -> String
summariseHPR i h = (show i) ++ ". \"" ++ (name h) ++ "\"; BL = " ++ (show $ maxLoss h) ++ ";" ++ (succinctList $ trades h) ++ ";" ++ (succinctList $ prices h)

succinctList :: (Show a) => [a] -> String
succinctList xs | length xs <= 4 = show xs
succinctList xs | otherwise      = "[" ++ (show $ xs !! 0) ++ "," ++ (show $ xs !! 1) ++ ",...," ++ (show $ xs' !! 1) ++ "," ++ (show $ xs' !! 0) ++ "]"
    where xs' = reverse xs