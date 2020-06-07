--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- Euclidian
--

module Euclidian
    ( euclDist
    , smallestEuclDist
    , biggestEuclDist
    ) where

import Coords

smallestEuclDist :: Reference -> Coords -> Coords -> Coords
smallestEuclDist ref coords1 coords2    | euclDist coords1 ref <= euclDist coords2 ref = coords1
                                        | otherwise = coords2

biggestEuclDist :: Reference -> Coords -> Coords -> Coords
biggestEuclDist ref coords1 coords2 | euclDist coords1 ref >= euclDist coords2 ref = coords1
                                    | otherwise = coords2

euclDist :: Coords -> Coords -> Float
euclDist (Position x1 y1 z1 _ _) (Position x2 y2 z2 _ _) = sqrt (((x1 - x2)^2) + ((y1 - y2)^2) + ((z1 - z2)^2))
