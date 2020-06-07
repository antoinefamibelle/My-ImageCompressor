--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- RemoveFarVectors
--

module FarVectors
    (
    ) where

import Coords
import Euclidian
import Closest

type Farthest = Coords

removeFarthest :: Reference -> Farthest -> [Coords] -> [Coords]
removeFarthest _ _ [] = []
removeFarthest ref ftht (n:ns) = smallestEuclDist ref n ftht : removeFarthest ref (biggestEuclDist ref n ftht) ns

removeNFarthest :: Int -> Reference -> [Coords] -> [Coords]
removeNFarthest loop ref lst    | loop <= 0 = lst
                                | otherwise = removeNFarthest (loop-1) ref $ removeFarthest ref ref lst

removeFarVectors :: Reference -> Int -> [Coords] -> [Coords]
removeFarVectors _ _ [] = error "need a complete coords list"
removeFarVectors ref loop lst = removeNFarthest loop ref lst

removeFarVectorFromFile :: FileName -> Reference -> Int -> IO [Coords]
removeFarVectorFromFile [] _ _ = error "need a complete FileName"
removeFarVectorFromFile fileName ref loop = readFile fileName >>= \ content ->
                                            return $ removeFarVectors ref loop $ createCoords $ lines content
