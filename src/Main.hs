{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad

import DynFlags
import FastString
import GHC
import GHC.Paths
import Lexer
import SrcLoc
import StringBuffer
import System.Environment

main :: IO ()
main = do
  file <- getArgs >>= \case
    [x] -> return x
    xs -> error $ "Expected 1 argument, got: " ++ show (length xs)
  content <- readFile file
  res <- runGhc (Just libdir) $
    return $ lexTokenStream
      (stringToStringBuffer content)
      (mkRealSrcLoc (mkFastString "a.hs") 0 0)
      unsafeGlobalDynFlags

  case res of
    POk _ tokens ->
      mapM_ (\(L srcSpan token) -> putStrLn $ show srcSpan ++ ": " ++ show token) tokens
    _ -> error "failed"
