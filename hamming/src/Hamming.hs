module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance (x:xs) (y:ys)
    | (length xs) /= (length ys) = Nothing
    | x == y                     = distance xs ys
    | otherwise                  = (+) <$> Just 1 <*> distance xs ys
