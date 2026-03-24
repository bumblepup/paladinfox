{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import Section
import System.Environment
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
  putStrLn "Running program ..."
  (f:_) <- getArgs
  cs <- LBS.toChunks <$> LBS.readFile f
  let result =
        case doparse cs $ parse readPDF of
          Failure b x -> "Error: (offset " ++ (show.offset.pos) b ++ ")\n" ++ showerr x
          Continue _ -> "Error: Unhandled continue"
          Success _ x -> "Success: " ++ show x
  putStrLn result

doparse :: [BS.ByteString] -> Result a -> Result a
doparse _ r@(Failure _ _) = r
doparse _ r@(Success _ _) = r
doparse [] (Continue f) = doparse [] $ f ""
doparse (x:xs) (Continue f) = doparse xs $ f x
