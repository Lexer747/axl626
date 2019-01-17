module CSV where

import Data.Csv
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Vector as V
import Data.List (isInfixOf)

import System.Directory

path :: String
path = "C:\\Users\\Lexer\\Documents\\Uni\\FYP\\axl626\\Application\\Data"

fileHeader :: String
fileHeader = "date,adj_close,adj_factor,adj_high,adj_low,adj_open,adj_volume\
         \,close,ex_dividend,high,low,open,split_ratio,volume"

type Fundamental = V.Vector (String, String, String, String, String, String, String, String,String, String, String, String, String, String)

files :: IO [String]
files = listDirectory path

filterFiles :: IO [String] -> IO [String]
filterFiles file = do
                     f <- file
                     return $ filter (isInfixOf "price") f

parseAll :: IO [Either String Fundamental]
parseAll = do
                f <- filterFiles files
                sequence $ map extractCsv f
    where extractCsv file = do
                               r <- readFile $ prependPath file
                               return $ parseCsv r

parseCsv :: String -> Either String Fundamental
parseCsv input = decode HasHeader (pack input)

prependPath :: String -> String
prependPath = ((path ++ "\\") ++)

checkForErrors :: IO [Either String Fundamental] -> IO [Fundamental]
checkForErrors parsed = 
    do
        p <- parsed
        let errors = [a | Left a <- p]
        case errors of
            [] -> return $ [a | Right a <- p]
            _  -> error $ show errors

