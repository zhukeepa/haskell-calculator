module Main where

import Prelude
import Calculate

main :: IO ()
main = do
  let fibs = 0:1:(zipWith (+) (tail fibs) fibs)
  putStrLn $ show $ take 10 fibs
