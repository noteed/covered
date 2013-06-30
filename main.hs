module Main (main) where

import Prelude

data Color = Red

main :: IO ()
main = do
  -- Always False.
  if 1 == 2
    then putStrLn "Nope."
    else putStrLn "Covered."
  if True
    then putStrLn "Covered."
    else putStrLn "Nope."
