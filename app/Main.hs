module Main where

import Brick
import Lib

ui :: Maybe String -> Widget ()
ui cn = str $ show cn

main :: IO ()
main = do
  cn <- getCommonName "192.30.253.113" "443"
  simpleMain $ ui cn
