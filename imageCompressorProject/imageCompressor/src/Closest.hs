--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- Closest
--

module Closest
    ( closest
    , parsingVectors
    , getClosestRef
    ) where

import Coords
import Euclidian

getClosestRef :: Coords -> [Centroid]  -> Reference
getClosestRef coords (n:ns) | ns == [] = theRef
                            | otherwise = smallestEuclDist coords theRef $ getClosestRef coords ns
                            where theRef = fst n

parsingVectors :: Reference -> [String] -> Coords
parsingVectors ref (n:ns)   | ns == [] = getCoords n
                            | otherwise = smallestEuclDist ref (getCoords n) $ parsingVectors ref ns

closest :: FileName -> Reference -> IO Coords
closest [] _ = error "need a complete FileName"
closest fileName ref =  readFile fileName >>= \ content ->
                        return $ parsingVectors ref $ lines content
