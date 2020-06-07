--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- ImageCompressorLib
--

module Coords where

import Prelude hiding (map)
import Data.Text hiding (dropWhile, all, lines, tail, head)
import Text.Read
import Data.Char

data Coords = Position  { r :: Float
                        , g :: Float
                        , b :: Float
                        , x :: Int
                        , y :: Int
                        } deriving (Show, Eq)

type FileName = String
type Reference = Coords
type Refs = [Reference]
type Centroid = (Reference, [Coords])
type Convergence = Float

size :: [String] -> Int
size [] = 0
size (_:xs) = 1 + size xs

isValidNum :: Maybe Float -> Bool
isValidNum Nothing = False
isValidNum num = True

isFloat :: String -> Bool
isFloat ""  = False
isFloat "." = False
isFloat ('.':xs) = False
isFloat xs  = case dropWhile isDigit xs of
                ""       -> True
                ('.':[]) -> False
                ('.':ys) -> all isDigit ys
                _        -> False

isNum :: String -> Bool
isNum "" = False
isNum xs = all isDigit xs

xVal :: [String] -> String
xVal [] = ""
xVal (x:xs) | isNum x == True = x
            | otherwise = xVal xs

yVal :: [String] -> String
yVal [] = ""
yVal (x:xs) | isNum x == True  = xVal xs
            | otherwise = yVal xs

rVal :: [String] -> String
rVal [] = ""
rVal (x:xs) | isFloat x == True = x
            | otherwise = rVal xs

gVal :: [String] -> String
gVal [] = ""
gVal (x:xs) | isFloat x == True = rVal xs
            | otherwise = gVal xs

bVal :: [String] -> String
bVal [] = ""
bVal (x:xs) | isFloat x == True = gVal xs
            | otherwise = bVal xs

readFloat :: String -> Float
readFloat [] = 0
readFloat str = read str::Float

readInt :: String -> Int
readInt [] = 0
readInt str = read str::Int

unpackTextList :: [Text] -> [String]
unpackTextList [] = []
unpackTextList (x:xs) = unpack x : unpackTextList xs

-- string (0,1)
-- return tuple = (0,1)
getPoints :: String -> (Int, Int)
getPoints [] = error "Cannot get points from empty string"
getPoints s | size splited == 4 = (readInt $ xVal splited, readInt $ yVal splited)
            | otherwise = error "Error string format"
            where splited = spliting ["(", ")", ","] [s]

-- string = (33,18,109)
-- tuple <=> string = (0,1)
-- return Position {r = 33.0, g = 18.0, b = 109.0, x = x, y = y}
toCoords :: String -> (Int, Int) -> Coords
toCoords [] _ = error "Cannot convert to coords empty string"
toCoords s (x, y)   | size splited == 5 = (Position (readFloat $ rVal splited) (readFloat $ gVal splited) (readFloat $ bVal splited) x y)
                    | otherwise = error "Error string format" 
                    where splited = spliting ["(", ")", ","] [s]

-- string = (0,1) (33,18,109)
-- return Position {x = 33.0, y = 18.0, z = 109.0}
getCoordsFromList :: [String] -> Coords
getCoordsFromList (points:coords:_) = toCoords coords $ getPoints points

getCoords :: String -> Coords
getCoords s | size splited == 2 = getCoordsFromList splited
            | otherwise = error "File Error string format"
                where   splited = spliting [" "] [s]

spliting :: [String] -> [String] -> [String]
spliting [] s2 = s2
spliting (x:xs) s2 = spliting xs $ forEach s2 x (\ x -> \ s -> unpackTextList (splitOn (pack s) (pack x)))

forEach :: [String] -> String -> (String -> String -> [String]) -> [String]
forEach [] _ _ = []
forEach (x:xs) s f = f x s ++ forEach xs s f

--

createCoords :: [String] -> [Coords]
createCoords [] = []
createCoords (n:ns) = getCoords n : createCoords ns

getCoordsFromFile :: FileName -> IO [Coords]
getCoordsFromFile fileName =    readFile fileName >>= \ content ->
                                return $ createCoords $ lines content

--