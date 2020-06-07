--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- ImageCompression
--

module ImageCompression
    ( compressing
    ) where

import System.Exit
import Coords
import Closest
import Euclidian
import Text.Printf
import System.Random

-- List de coordonnées
-- sur chaque coordonnée, on va chercher la reference la plus proche
-- <=> getClosestRef -> la ref la plus proche
-- -> ajouter la coordonnée à la ref la plus proche <=> l'ajouter au centroid
-- -> parser un tableau de centroid 
-- -> si on trouve la reference, on l'ajoute a son tableau de coords
-- -> si on trouve pas la reference, on l'ajoute au tableau de centroid, puis on ajoute la coords au tableau de la reference
-- On calcule le nouveau centroid
-- On compare les ancien centroids aux nouveaux centroid
-- si convergence pas suffisante, on rebloque
-- si convergence suffisante, on print puis on return ExitSuccess

-- NEED TO REMOVE THE INT + JUST FOR TEST

randomIntNumber :: IO Int
randomIntNumber =   newStdGen >>= \ gen ->
                    return $ (abs $ head $ (randoms gen :: [Int])) `mod` 256

randomCoords :: IO Coords
randomCoords =  randomIntNumber >>= \ r ->
                randomIntNumber >>= \ g ->
                randomIntNumber >>= \ b ->
                return $ (Position (fromIntegral r :: Float) (fromIntegral g :: Float) (fromIntegral b :: Float) 0 0)

createCentroid :: Int -> IO [Centroid]
createCentroid 0 = return []
createCentroid n = do   rand <- randomCoords
                        otherVals <- createCentroid (n-1)
                        return $ ((rand, []) : otherVals)

--

isConvergence :: Convergence -> [Centroid] -> [Centroid] -> Bool
isConvergence e [] [] = True
isConvergence e [] _ = error "Basic centroid number error"
isConvergence e _ [] = error "New centroid number error"
isConvergence e ((ref1, _):xs1) ((ref2, _):xs2) | distance <= e && distance >= 0 = isConvergence e xs1 xs2
                                                | otherwise = False
                                                where distance = euclDist ref1 ref2

--

printRef :: Reference -> IO ExitCode
printRef (Position r g b _ _) = (printf "--\n(%.0f,%.0f,%.0f)\n-\n" r g b) >>
                                return ExitSuccess

printCoords :: [Coords] -> IO ExitCode
printCoords [] = return $ ExitSuccess
printCoords ((Position r g b x y):xs) = (printf "(%d,%d) (%0.f,%0.f,%0.f)\n" x y r g b) >>
                                        printCoords xs

printCentroids :: Centroid -> IO ExitCode
printCentroids (ref, allCoords) =   printRef ref >>
                                    printCoords allCoords

printCtrdAndReturn :: [Centroid] -> IO ExitCode
printCtrdAndReturn [] = return ExitSuccess
printCtrdAndReturn (x:xs) = printCentroids x >>
                            printCtrdAndReturn xs

--
--- --- --- --- ---

type SumR = Float
type SumG = Float
type SumB = Float
type Size = Float

parseCoords :: [Coords] -> (SumR, SumG, SumB, Size) -> (SumR, SumG, SumB, Size)
parseCoords [] sums = sums
parseCoords ((Position r g b _ _):xs) (sumR, sumG, sumB, size) = parseCoords xs (sumR+r, sumG+g, sumB+b, size+1)

newCentroids :: (SumR, SumG, SumB, Size) -> Centroid
newCentroids (sumR, sumG, sumB, size) = ((Position ((sumR/size)) (sumG/size) (sumB/size) 0 0), [])

makeCentroidsFrom :: [Centroid] -> [Centroid]
makeCentroidsFrom [] = []
makeCentroidsFrom (ctrd@(_, []):xs) = ctrd : makeCentroidsFrom xs
makeCentroidsFrom ((_, allCoords):xs) = newCentroids (parseCoords allCoords (0, 0, 0, 0)) : makeCentroidsFrom xs

--- --- --- --- ---
--

add :: Coords -> Centroid -> Centroid
add coords (ref, allCoords) = (ref, coords : allCoords)

isSameVal :: Float -> Float -> Bool
isSameVal x1 x2 | x1 >= x2 && (x1-1) < x2 = True
                | otherwise = False

isSame :: Centroid -> Reference -> Bool
isSame ((Position r1 g1 b1 _ _), _) (Position r2 g2 b2 _ _) | isSameVal r1 r2 && isSameVal g1 g2 && isSameVal b1 b2 = True
                                                            | otherwise = False

addClosest :: [Centroid] -> Reference -> Coords -> [Centroid]
addClosest [] _ _ = []
addClosest (x:xs) ref crds  | isSame x ref == True = add crds x : xs
                            | otherwise = x : addClosest xs ref crds

completeCtrdWithCoords :: [Centroid] -> [Coords] -> [Centroid]
completeCtrdWithCoords [] _ = error "Need centroids"
completeCtrdWithCoords ctrd [] = ctrd
completeCtrdWithCoords ctrd (x:xs) = completeCtrdWithCoords (addClosest ctrd (getClosestRef x ctrd) x) xs

makeTheCompression :: [Centroid] -> Convergence -> [Coords] -> IO ExitCode
makeTheCompression [] _ _ = error "Need centroids"
makeTheCompression _ _ [] = error "Need points"
makeTheCompression ctrd e coords    | isConvergence e ctrd newCentroids == True = printCtrdAndReturn ctrdWCoords
                                    | otherwise = makeTheCompression newCentroids e coords
                                    where   ctrdWCoords = completeCtrdWithCoords ctrd coords
                                            newCentroids = makeCentroidsFrom ctrdWCoords


--

compressing :: Int -> Convergence -> String -> IO ExitCode
compressing 0 _ _ = return $ ExitFailure 84
compressing _ 0 _ = return $ ExitFailure 84
compressing _ _ [] = return $ ExitFailure 84
compressing n e path =  getCoordsFromFile path >>= \ content ->
                        createCentroid n >>= \ ctrds ->
                        makeTheCompression ctrds e content

