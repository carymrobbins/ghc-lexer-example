{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Char
import Data.IORef
import System.Exit
import System.IO
import Text.Read

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

  flags <- runGhc (Just libdir) $ return unsafeGlobalDynFlags

  let stringBuf = stringToStringBuffer content
  let srcLoc = mkRealSrcLoc (mkFastString "a.hs") 0 0
  let initialState = mkPState flags stringBuf srcLoc
  pStateRef <- newIORef initialState

  hSetBuffering stdout NoBuffering

  putStrLn "Press enter for next element. Anything else will exit immediately"

  let loop :: IO ()
      loop = (map toLower <$> getLine) >>= \case
        "" -> do
          pState <- readIORef pStateRef
          case unP (lexer False return) pState of
            POk pState' ltok -> do
              writeIORef pStateRef pState'
              case ltok of
                L _ ITeof -> putStrLn "end of file, we're done!"
                L srcSpan token -> do
                  putStrLn $ show srcSpan ++ ": " ++ show token
                  loop

            PFailed srcSpan msgDoc -> do
              hPutStrLn stderr $ "Lexer failed: " ++ show srcSpan {- ++ ": " ++ show msgDoc -}
              exitFailure

        _ -> putStrLn "exiting..."

  loop
