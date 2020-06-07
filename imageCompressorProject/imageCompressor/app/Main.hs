--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- Main
--

module Main where

import Errors
import Coords
import ImageCompression
import System.Exit
import System.Environment

main :: IO ()
main =  do  args <- getArgs
            exitstatus <- isErrors args
            case exitstatus of
                ExitSuccess ->  launchProgram args >>= \ exitstatusLaunching ->
                                exitWith exitstatusLaunching
                _ -> exitWith exitstatus

readingColorNumber :: String -> Int
readingColorNumber [] = 0
readingColorNumber n = readInt n

readingConvergence :: String -> Float
readingConvergence [] = 0
readingConvergence e = readFloat e

launchProgram :: [String] -> IO ExitCode
launchProgram [] = return $ ExitFailure 84
launchProgram (n:e:path:_) = compressing (readingColorNumber n) (readingConvergence e) path
