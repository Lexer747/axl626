module SemiDate where

-- "YYYY-MM-DD"

type CompareFunc = Integer -> Integer -> Bool
data Date = Year | Month | Day

compareYear :: CompareFunc -> String -> String -> Bool
compareYear f (x1:x2:x3:x4:_) (y1:y2:y3:y4:_) = f x y
    where x = read (x1:x2:x3:x4:[])
          y = read (y1:y2:y3:y4:[])
compareYear _ _ _ = False

compareMonth :: CompareFunc -> String -> String -> Bool
compareMonth f (_:_:_:_:_:x1:x2:_) (_:_:_:_:_:y1:y2:_) = f x y
    where x = read (x1:x2:[])
          y = read (y1:y2:[])
compareMonth _ _ _ = False

compareDay :: CompareFunc -> String -> String -> Bool
compareDay f (_:_:_:_:_:_:_:_:x1:x2:_) (_:_:_:_:_:_:_:_:y1:y2:_) = f x y
    where x = read (x1:x2:[])
          y = read (y1:y2:[])
compareDay _ _ _ = False

--Roughly equal check, the first arg determines how far out it can be and still be equal
almostEq :: (Eq a, Enum a) => Integer -> a -> a -> Bool
almostEq 0 x y             = x == y
almostEq _ x y | x == y    = True
almostEq n x y | otherwise = almostEq (n - 1) (succ x) y

findBasedOnDate :: [(CompareFunc, Date)] -> (Bool -> Bool -> Bool) -> String -> String -> Bool
findBasedOnDate fs combine x y = my_fold combine compared
    where compared = map (\(f,d) -> applyBasedOnDate f d x y) fs

my_fold :: (a -> a -> a) -> [a] -> a
my_fold _ []     = error "fold called with empty list"
my_fold _ (x:[]) = x
my_fold f (x:x':xs) = my_fold_help (x':xs) f (f x x')

my_fold_help :: [a] -> (a -> a -> a) -> a -> a
my_fold_help []        _ _   = error "fold called with 0 args"
my_fold_help (x:[])    f acc = f x acc
my_fold_help (x:xs) f acc = my_fold_help xs f (f x acc)


applyBasedOnDate :: CompareFunc -> Date -> String -> String -> Bool
applyBasedOnDate f Year  = compareYear f
applyBasedOnDate f Month = compareMonth f
applyBasedOnDate f Day   = compareDay f

--sort dates:
--if f x y then false 
--else !(f x y) || (== x y) then true
sortDate :: CompareFunc -> String -> String -> Bool
sortDate f x y =
    if compareYear f x y
        then True
        else if compareYear (==) x y
                 then if compareMonth f x y
                          then True
                          else if compareMonth (==) x y
                                   then if compareDay f x y
                                            then True
                                            else False
                                   else False
                 else False