module SemiDate where

-- "YYYY-MM-DD"

type CompareFunc = Integer -> Integer -> Bool

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