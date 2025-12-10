{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toUpper)
import Data.Traversable (for)
import System.Environment (getArgs)
import Text.Read (readMaybe)

data Direction = LeftDir | RightDir
  deriving (Show, Eq)

data RotationInstruction = RotationInstruction Direction Integer
  deriving (Show)

type ParseResult a = ExceptT String IO a

parseInstructions :: FilePath -> ParseResult [RotationInstruction]
parseInstructions filePath = do
  content <- liftIO $ lines <$> readFile filePath

  for (filter (not . null) content) $ \line -> do
    case line of
      directionChar : amountStr -> do
        direction <- case toUpper directionChar of
          'L' -> pure LeftDir
          'R' -> pure RightDir
          _ -> throwError "Invalid direction, must be either L or R"

        amount <- case readMaybe amountStr of
          Just amount -> pure amount
          _ -> throwError "Invalid direction value"

        pure $ RotationInstruction direction amount
      _ -> throwError ("Invalid line " <> line)

maxDialValue :: Integer
maxDialValue = 99

traverseInstructions :: Integer -> Integer -> [RotationInstruction] -> Integer
traverseInstructions zeros _ [] = zeros
traverseInstructions zeros start (RotationInstruction dir amount : remaining) =
  traverseInstructions nextZeros nextValue remaining
  where
    rotationValue :: Integer
    rotationValue = if dir == LeftDir then -amount else amount

    nextValue :: Integer
    nextValue = (start + rotationValue) `mod` (maxDialValue + 1)

    nextZeros :: Integer
    nextZeros = zeros + if nextValue == 0 then 1 else 0

main :: IO ()
main = do
  getArgs >>= \case
    [filePath] -> do
      runExceptT (parseInstructions filePath) >>= \case
        Left errMsg -> do
          putStrLn errMsg
        Right instructions -> do
          print $ traverseInstructions 0 50 instructions
    _ -> do
      putStrLn "Usage: ./day1.hs <input_file_path>"
