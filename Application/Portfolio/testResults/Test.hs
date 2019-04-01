module Test where

import Data.Maybe (mapMaybe)

f :: String -> IO [String]
f path = do
    file <- readFile path
    return $ lines $ filter (\c -> not $ c `elem` badChars ) file

badChars :: [Char]
badChars = ['\160','\9632','\9618','\r','\NUL']

files :: [String]
files = map (\n -> "raw" ++ (show n) ++ ".csv") [0..9 :: Int]

allCSV :: IO ()
allCSV = do
    xss <- mapM f files
    let out = interweave xss
    writeFile "temp.csv" $ unlines out

safeHd :: [a] -> Maybe a
safeHd (x:_) = Just x
safeHd []    = Nothing

safeTl :: [a] -> Maybe [a]
safeTl (_:xs) = Just xs
safeTl []     = Nothing

interweave :: [[a]] -> [a]
interweave xs = (mapMaybe safeHd xs) ++ (recCall)
    where recCall = case mapMaybe safeTl xs of
                        [] -> []
                        x -> interweave x
