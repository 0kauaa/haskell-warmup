{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

--
-- Complete the 'compareTriplets' function below.
--
-- The function is expected to return an INTEGER_ARRAY.
-- The function accepts following parameters:
--  1. INTEGER_ARRAY a
--  2. INTEGER_ARRAY b
--

{-
def compareTriplets(a, b):
    apoints = 0
    bpoints = 0
    for i in range(len(a)):
        if a[i] > b[i]:
            apoints+=1
        
        if a[i] < b[i]:
            bpoints+=1

        else:
            apoints+=0
            bpoints+=0
            
    return [apoints, bpoints]
-}

compareTriplets :: [Int] -> [Int] -> [Int]
compareTriplets a b = Data.List.foldl updatePoints [0, 0] (Data.List.zip a b)
  where
    updatePoints [apoints, bpoints] (x, y)
      | x > y     = [apoints + 1, bpoints]
      | x < y     = [apoints, bpoints + 1]
      | otherwise = [apoints, bpoints]
    
lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    aTemp <- getLine

    let a = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip aTemp

    bTemp <- getLine

    let b = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip bTemp

    let result = compareTriplets a b

    hPutStrLn fptr $ Data.List.intercalate " " $ Data.List.map (\x -> show x) $ result

    hFlush fptr
    hClose fptr
