--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- Errors
--

module Errors
    (isErrors
    ) where

import Coords
import System.Exit
import Data.Char
import System.Directory

checkNumberOfColors :: String -> IO ExitCode
checkNumberOfColors [] = return $ ExitFailure 84
checkNumberOfColors str = case all isDigit str of
                            True -> return ExitSuccess
                            _ -> return $ ExitFailure 84

checkConvergence :: String -> IO ExitCode
checkConvergence [] = return $ ExitFailure 84
checkConvergence str = case isFloat str of
                        True -> return ExitSuccess
                        _ -> return $ ExitFailure 84

checkPath :: String -> IO ExitCode
checkPath [] = return $ ExitFailure 84
checkPath str = doesFileExist str >>= \ doesExit ->
                case doesExit of
                    True -> return ExitSuccess
                    _ -> return $ ExitFailure 84

returnVals :: [ IO ExitCode] -> IO ExitCode
returnVals [] = return ExitSuccess
returnVals (x:xs) = x >>= \ cExitCode ->
                    case cExitCode of
                        ExitSuccess -> returnVals xs
                        _ -> return $ ExitFailure 84

checkIsGoodParams :: [String] -> IO ExitCode
checkIsGoodParams [] = return $ ExitFailure 84
checkIsGoodParams (n:e:path:_) = returnVals [checkNumberOfColors n, checkConvergence e, checkPath path]


isErrors :: [String] -> IO ExitCode
isErrors [] = return $ ExitFailure 84
isErrors strs   | size strs == 3 = checkIsGoodParams strs
                | otherwise = return $ ExitFailure 84