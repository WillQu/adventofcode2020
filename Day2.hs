module Day2 (runDay2) where

import Control.Exception

data PasswordRule = PasswordRule
    { minLetters :: Int
    , maxLetters :: Int
    , letter :: Char
    }

readRule :: String -> PasswordRule
readRule s =
    let
        (minL, (_:rest)) = break (== '-') s
        (maxL, (_:c:_)) = break (== ' ') rest
    in assert (not $ null s) $ PasswordRule (read minL) (read maxL) c

readPassword :: String -> String
readPassword = last . words

check :: PasswordRule -> String -> Bool
check rule s =
    let l = length (filter (== (letter rule)) s)
    in l >= minLetters rule && l <= maxLetters rule

xor :: Bool-> Bool-> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

check2 :: PasswordRule -> String -> Bool
check2 rule s = (s !! (minLetters rule - 1) == letter rule) `xor` (s !! (maxLetters rule - 1) == letter rule)

solveLine :: String -> Bool
solveLine "" = False
solveLine s = check (readRule s) (readPassword s)

solve :: String -> Int
solve = length . (filter solveLine) . lines

solveLine2 :: String -> Bool
solveLine2 "" = False
solveLine2 s = check2 (readRule s) (readPassword s)

solve2 :: String -> Int
solve2 = length . (filter solveLine2) . lines

runDay2 :: IO ()
runDay2 = 
    do
        x <- readFile "passwords"
        print $ solve x
        print $ solve2 x
