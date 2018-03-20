{-# LANGUAGE ConstraintKinds #-}

module Upcast.IO (
  module System.IO
, ASCIIColor(..)
, applyColor
, oops
, expect
, srsly
, warn
, warn8
) where

import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           System.Exit (ExitCode(..))
import           Control.Exception

import qualified Data.ByteString.Char8 as B8

data ASCIIColor = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
                deriving (Enum)

needsColor :: Bool
needsColor = unsafePerformIO $ hIsTerminalDevice stderr

applyColor :: ASCIIColor -> String -> String
applyColor color s = case needsColor of
                         True -> "\ESC[1;" ++ colorCode ++ "m" ++ s ++ "\ESC[0m"
                         False -> s
  where
    colorCode = show $ 30 + fromEnum color


oops :: String -> IO a
oops = throwIO . ErrorCall

expect :: Eq a => a -> String -> IO a -> IO ()
expect value excuse action = do
  result <- action
  case result of
      x | x == value -> return ()
      _ -> oops excuse

srsly :: String -> IO ExitCode -> IO ()
srsly = expect ExitSuccess

warn :: [String] -> IO ()
warn = hPutStrLn stderr . mconcat

warn8 :: [B8.ByteString] -> IO ()
warn8 = B8.hPutStrLn stderr . mconcat
