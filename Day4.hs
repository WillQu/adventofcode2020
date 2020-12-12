module Day4 (runDay4) where

import Data.List.Split
import Text.Regex.TDFA

mandatory :: [String]
mandatory = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validLine :: String -> Bool
validLine line = all (`elem` (map (take 3)) (words line)) mandatory

validLine2 :: String -> Bool
validLine2 line = validLine line && all validWord ws
    where ws = words line

countValid :: String -> Int
countValid s = length $ filter validLine $ splitOn "\n\n" s

countValid2 :: String -> Int
countValid2 s = length $ filter validLine2 $ splitOn "\n\n" s

runDay4 :: IO ()
runDay4 = do
    content <- readFile "passports"
    print $ countValid content
    print $ countValid2 content

validWord :: String -> Bool
validWord s =
    let
        key = take 3 s
        value = drop 4 s
        (_ , _, _, heightResults) = value =~ "^([0-9]+)(in|cm)$" :: (String, String, String, [String])
        heightOK = case heightResults of
            (x:"cm":_) -> read x >= 150 && read x <= 193
            (x:"in":_) -> read x >= 59 && read x <= 76
            _ -> False
        result = case key of
            "byr" -> value =~ "^[0-9]{4}$" && read value >= 1920 && read value <= 2002
            "iyr" -> value =~ "^[0-9]{4}$" && read value >= 2010 && read value <= 2020
            "eyr" -> value =~ "^[0-9]{4}$" && read value >= 2020 && read value <= 2030
            "hgt" -> heightOK
            "hcl" -> value =~ "^#[0-9a-f]{6}$"
            "ecl" -> value =~ "^amb|blu|brn|gry|grn|hzl|oth$"
            "pid" -> value =~ "^[0-9]{9}$"
            _ -> True
    in
        result
