module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString Nothing Nothing = unlines ["_ _ _ _ _ _ _ _",
                                       "_ _ _ _ _ _ _ _",
                                       "_ _ _ _ _ _ _ _",
                                       "_ _ _ _ _ _ _ _",
                                       "_ _ _ _ _ _ _ _",
                                       "_ _ _ _ _ _ _ _",
                                       "_ _ _ _ _ _ _ _",
                                       "_ _ _ _ _ _ _ _"]

boardString white black = replaceAt white "W" . replaceAt black "B" $ boardString Nothing Nothing
    where replaceAt Nothing _ str = str
          replaceAt (Just (a, b)) c str = 
              let idx = (a * 16 + ((filter even [0..]) !! b))
                  splitStr = splitAt idx str 
              in  (fst splitStr) ++ c ++ (tail . snd $ splitStr)

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack queenA queenB = isCard || isDiag 
    where isCard = fst queenA == fst queenB || snd queenA == snd queenB
          isDiag = queenA `elem` (diags queenB)
              where diags (a, b) = foldl (\acc i -> acc ++ [(x, y) | x <- [a + i, a - i], 
                                                                     y <- [b + i, b - i], 
                                                                     x `elem` [1..8], 
                                                                     y `elem` [1..8]]) [] [1..8]
