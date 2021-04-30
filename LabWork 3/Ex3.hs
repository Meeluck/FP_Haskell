import Data.Char

getCode ::  Char -> Int
getCode c = ord c - ord 'a'

getSymbol :: Int -> Char
getSymbol n = chr (ord 'a' + n)

keygen ::  String -> [Int] -> [Int]
keygen msg key = concat (replicate n key) 
    where 
        n = (length msg `div` length key) + 1

vigenere ::  String -> [Int] -> String
vigenere msg key = [getSymbol (((getCode m) + k) `mod` 26) | (m,k)<-zip msg (keygen msg key)]

deVigenere ::  String -> [Int] -> String
deVigenere msg key = [getSymbol (((getCode m) - k) `mod` 26) | (m,k)<-zip msg (keygen msg key)]

