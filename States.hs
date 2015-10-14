
getState :: Bool -> Int -> Int -> Int
getState isSoft playerCount dealerCount = (isSoft * 160) + ((playerCount - 4) + ((dealerCount-1) * 16)


initialPolicy :: [Int]
initialPolicy = take 320 (repeat 0)
