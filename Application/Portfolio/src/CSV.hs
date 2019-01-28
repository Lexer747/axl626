module CSV 
    (
    Fundamental,
    parseAll,
    checkForErrors
    ) where

import Data.Csv
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Vector as V
import Data.List (isInfixOf)

import System.Directory

--hardcoded path to directory containing csv files
path :: String
path = "C:\\Users\\Lexer\\Documents\\Uni\\FYP\\axl626\\Application\\Data"

--The format we are expecting CSV to be in
--fileHeader :: String
--fileHeader = "date,adj_close,adj_factor,adj_high,adj_low,adj_open,adj_volume\
--         \,close,ex_dividend,high,low,open,split_ratio,volume"

-- The acceptable haskell type representing our CSV
type Fundamental = V.Vector (String, String, String, String, String, String, String, String,String, String, String, String, String, String)

--List all files in the directory
files :: IO [String]
files = listDirectory path

--Only use the CSV files with the correct data in
filterFiles :: IO [String] -> IO [String]
filterFiles file = do
                     f <- file
                     return $ filter (isInfixOf "price") f

--Given a string, from a file, put it into haskell type
parseCsv :: String -> Either String Fundamental
parseCsv input = decode HasHeader (pack input)

--Make a relative file, hardcoded to the base directory
prependPath :: String -> String
prependPath = ((path ++ "\\") ++)

--check if we had any parse errors reading the file, catastrophically fail with
--the error message if we did
checkForErrors :: IO [(String, String, Either String Fundamental)] -> IO [(String, String, Fundamental)]
checkForErrors parsed = 
    do
        parse <- parsed
        let errors = [a | (_,_,Left a) <- parse]
        case errors of
            [] -> return $ [(p,n,a) | (p,n,Right a) <- parse]
            _  -> error $ show errors

-- Actually read the files and store them as haskell types
parseAll :: IO [(String, String, Either String Fundamental)]
parseAll = do
                f <- filterFiles files --get all the files
                sequence $ map extractCsv f
                --sequence takes a list of Monad m => [m x] -> m [x]
                --useful for all the IO wrapped file reads
    where extractCsv file = do
                               r <- readFile $ prependPath file
                               return $ ((prependPath file),file,parseCsv r)