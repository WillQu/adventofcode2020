module Day3 (runDay3) where

readInput :: IO [[Char]]
readInput = do
    content <- readFile "slopes"
    return $ map cycle $ lines content

slide :: Int -> Int -> [[Char]] -> Int
slide right down [] = 0
slide right down content@((x:_):_) = inc + (slide right down $ drop down $ map (drop right) content)
    where inc = case x of
            '#' -> 1
            _ -> 0

slide2 :: [[Char]] -> (Int, Int) -> Int
slide2 content (x, y) = slide x y content

runDay3 :: IO ()
runDay3 = do
    content <- readInput
    print $ slide 3 1 content
    print . product $ map (slide2 content) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
