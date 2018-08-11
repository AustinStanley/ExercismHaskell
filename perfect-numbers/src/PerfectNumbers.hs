module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

factorize :: Int -> [Int]
factorize n = [x | x <- [1..n-1], n `mod` x == 0]

classify :: Int -> Maybe Classification
classify n
    | n <= 0   = Nothing
    | res == n = Just Perfect
    | res < n  = Just Deficient
    | res > n  = Just Abundant
    where res = sum . factorize $ n
